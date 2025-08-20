import http from 'k6/http';
import { check, sleep } from 'k6';
import { Trend, Rate, Counter } from 'k6/metrics';

/** ---------- 환경 변수 ---------- **/
const BASE_URL   = __ENV.BASE_URL   || 'http://localhost:8080';
const AUTH       = __ENV.AUTH || ''; // 예: "Bearer xxx", 없으면 헤더 미설정
const DURATION   = __ENV.DURATION || '60s';

// 초당 요청 속도(RPS) 목표
const PRICE_RPS  = Number(__ENV.PRICE_RPS || 10);
const ORDER_RPS  = Number(__ENV.ORDER_RPS || 40);

// 경합 유도용 POC ID 목록(쉼표로 구분). 예: "1,1,1,1" → 같은 ID만 계속 때리기
const POC_IDS    = (__ENV.POC_IDS || '1,1,1,1').split(',').map(s => Number(s.trim()));

// 주문 수량 & 가격 범위
const QTY        = Number(__ENV.QTY || 1);
const PRICE_MIN  = Number(__ENV.PRICE_MIN || 1000);
const PRICE_MAX  = Number(__ENV.PRICE_MAX || 20000);

/** ---------- 시나리오/옵션 ---------- **/
export const options = {
    discardResponseBodies: true,
    scenarios: {
        price_changes: {
            executor: 'constant-arrival-rate',
            duration: DURATION,
            rate: PRICE_RPS, timeUnit: '1s',
            preAllocatedVUs: Math.max(1, Math.ceil(PRICE_RPS / 2)),
            maxVUs: 100,
            exec: 'priceWorker',
        },
        order_creates: {
            executor: 'constant-arrival-rate',
            duration: DURATION,
            rate: ORDER_RPS, timeUnit: '1s',
            preAllocatedVUs: Math.max(1, Math.ceil(ORDER_RPS / 2)),
            maxVUs: 100,
            exec: 'orderWorker',
        },
    },
    // thresholds: {
    //     // 전체 실패율(4xx/5xx). improved에서는 거의 0에 가깝게, baseline은 더 클 수 있어요.
    //     'http_req_failed{type:price}': ['rate<0.05'],
    //     'http_req_failed{type:order}': ['rate<0.15'],
    //
    //     // p95 지연 시간. 환경에 맞게 조정하세요.
    //     'http_req_duration{type:price}': ['p(95)<800'],
    //     'http_req_duration{type:order}': ['p(95)<1500'],
    //
    //     // 체크 성공률
    //     'checks{type:price}': ['rate>0.98'],
    //     'checks{type:order}': ['rate>0.95'],
    // },
};

/** ---------- 커스텀 메트릭(선택) ---------- **/
const order429 = new Counter('order_429');
const order409 = new Counter('order_409');
const order5xx = new Counter('order_5xx');
const price5xx = new Counter('price_5xx');
const tPrice   = new Trend('t_price');
const tOrder   = new Trend('t_order');

/** ---------- 유틸 ---------- **/
function rndInt(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}
function pickPocId() {
    return POC_IDS[Math.floor(Math.random() * POC_IDS.length)];
}
function headers(extra = {}) {
    const base = { 'Content-Type': 'application/json' };
    if (AUTH) base['Authorization'] = AUTH;
    return Object.assign(base, extra);
}

/** ---------- 워커: 가격 변경 ---------- **/
export function priceWorker() {
    const pocId = pickPocId();
    const newPrice = rndInt(PRICE_MIN, PRICE_MAX);

    const url = `${BASE_URL}/bm/price/${pocId}`;
    const res = http.post(url, JSON.stringify({ newPrice }), {
        headers: headers(),
        tags: { type: 'price', endpoint: '/bm/price' },
    });

    // improved(비동기)면 202, baseline(동기)면 200/204가 나올 수 있음
    const ok = check(res, {
        'price: status is expected': r => [200, 202, 204].includes(r.status),
    }, { type: 'price' });

    if (res.status >= 500) price5xx.add(1);
    tPrice.add(res.timings.duration);

}

/** ---------- 워커: 주문 생성 ---------- **/
export function orderWorker() {
    const pocId = pickPocId();
    const url = `${BASE_URL}/bm/orders`;
    const body = JSON.stringify({ pocIdList: [pocId], quantity: QTY });

    const res = http.post(url, body, {
        headers: headers(),
        tags: { type: 'order', endpoint: '/bm/orders' },
    });

    // improved는 202 예상, baseline은 200/204가 나올 수 있음.
    // 혼잡 시 409/429가 섞여도 "예상된 현상"으로 간주하려면 체크엔 포함하지 않습니다.
    const ok = check(res, {
        'order: status is expected (2xx/202)': r => [200, 202, 204].includes(r.status),
    }, { type: 'order' });

    if (res.status === 429) order429.add(1);
    if (res.status === 409) order409.add(1);
    if (res.status >= 500)  order5xx.add(1);

    tOrder.add(res.timings.duration);
    // sleep(0.01);
}

/** ---------- 요약 리포트 파일 ---------- **/
export function handleSummary(data) {
    return {
        'summary.json': JSON.stringify(data, null, 2),
    };
}