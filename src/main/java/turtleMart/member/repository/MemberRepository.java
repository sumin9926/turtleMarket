package turtleMart.member.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.member.entity.Member;

import java.util.Optional;

public interface MemberRepository extends JpaRepository<Member, Long> {
    boolean existsByEmail(String email);


    Optional<Member> findMemberByEmail(String email);
}
