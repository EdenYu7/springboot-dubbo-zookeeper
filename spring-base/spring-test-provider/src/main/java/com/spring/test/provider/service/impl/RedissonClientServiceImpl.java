package com.spring.test.provider.service.impl;

import com.spring.test.provider.service.RedissonClientService;
import org.redisson.api.RBucket;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class RedissonClientServiceImpl implements RedissonClientService {
	
	@Autowired
	private RedissonClient redissonClient;
	
	@Override
	public void addObject(String key, RBucket<Object> value) {
		RBucket<Object> bucket = redissonClient.getBucket(key);
		bucket.set(value);
	}

	@Override
	public void addStr(String key, String value) {
		RBucket<Object> bucket = redissonClient.getBucket(key);
		bucket.set(value);
	}

	@Override
	public void addStrAndTimestamp(String key, String value, long timestamp) {
		RBucket<Object> bucket = redissonClient.getBucket(key);
		bucket.set(value);
		bucket.expireAt(timestamp);
	}

	@Override
	public Object getObject(String key) {
		RBucket<Object> bucket = redissonClient.getBucket(key);
		Object obj = bucket.get();
		return obj;
	}

	@Override
	public String getStr(String key) {
		RBucket<String> bucket = redissonClient.getBucket(key);
		String obj = bucket.get();
		return obj;
	}

	@Override
	public long getExpire(String key) {
		RBucket<String> bucket = redissonClient.getBucket(key);
		long obj = bucket.remainTimeToLive();
		return obj;
	}
	
	@Override
	public RLock getLock(String key) {
		RLock lock = redissonClient.getLock(key);
		return lock;
	}


}
