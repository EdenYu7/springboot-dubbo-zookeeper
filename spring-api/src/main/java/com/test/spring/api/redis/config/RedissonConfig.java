package com.test.spring.api.redis.config;

import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.Config;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RedissonConfig {
	
	@Value("${spring.redis.host}")
    private String host;
	
	@Value("${spring.redis.port}")
    private Integer port;
	
	@Value("${spring.redis.password}")
    private String password;

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public Integer getPort() {
		return port;
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	@Bean
    public RedissonClient redissonClient() throws Exception {
		Config config = new Config();
		config.useSingleServer().setAddress("redis://" + host + ":" + port);
		config.useSingleServer().setPassword(password);
		return Redisson.create(config);
    }
	
}
