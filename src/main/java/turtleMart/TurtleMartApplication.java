package turtleMart;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import turtleMart.security.JwtSecurityProperties;

@EnableScheduling
@EnableJpaAuditing
@SpringBootApplication
@EnableConfigurationProperties({JwtSecurityProperties.class})
@EnableMethodSecurity(securedEnabled = true)
public class TurtleMartApplication {

	public static void main(String[] args) {
		SpringApplication.run(TurtleMartApplication.class, args);
	}

}
