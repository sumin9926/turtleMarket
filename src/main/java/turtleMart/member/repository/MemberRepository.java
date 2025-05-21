package turtleMart.member.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.member.entity.Member;

public interface MemberRepository extends JpaRepository<Member, Long> {
}
