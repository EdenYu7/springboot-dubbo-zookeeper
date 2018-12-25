package com.spring.test.provider;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.scheduling.annotation.EnableScheduling;

import java.net.UnknownHostException;

@MapperScan("com.spring.base.dao.mapper")
@SpringBootApplication(scanBasePackages={"com.spring"})
@ComponentScan(basePackages = "com.spring")
@EnableScheduling
//@EnableAutoConfiguration(exclude = {DataSourceAutoConfiguration.class})
public class SpringTestProviderApplication {

	public static void main(String[] args) {
		SpringApplication.run(SpringTestProviderApplication.class, args);
	}

/*	@Bean
	public RedisTemplate<String, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory) throws UnknownHostException {
		RedisTemplate<String, Object> template = new RedisTemplate();
		template.setConnectionFactory(redisConnectionFactory);
		template.setKeySerializer(new StringRedisSerializer());
		return template;
	}*/

	@Bean
	public RedisTemplate<String, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory) throws UnknownHostException {
		RedisTemplate<String, Object> template = new RedisTemplate();
		template.setConnectionFactory(redisConnectionFactory);
		template.setKeySerializer(new StringRedisSerializer());
		return template;
	}

}

