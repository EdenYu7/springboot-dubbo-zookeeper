package com.spring.test.provider.service;

import org.redisson.api.RBucket;
import org.redisson.api.RLock;

public interface RedissonClientService {
	
	/**
	 * 缓存一个对象
	 * 
	 * @param key
	 * @param value
	 *            对象必须继承RBucket
	 */
	public void addObject(String key, RBucket<Object> value);

	/**
	 * 缓存一个String
	 * 
	 * @param key
	 * @param value
	 */
	public void addStr(String key, String value);

	/**
	 * 缓存一个String并设置时效时间
	 * 
	 * @param key
	 * @param value
	 */
	public void addStrAndTimestamp(String key, String value, long timestamp);

	/**
	 * 获取一个缓存的对象
	 * 
	 * @param key
	 * @return
	 */
	public Object getObject(String key);

	/**
	 * 获取一个缓存的String
	 * 
	 * @param key
	 * @return
	 */
	public String getStr(String key);

	/**
	 * 获取一个缓存的String的有效期
	 * 
	 * @param key
	 * @return
	 */
	public long getExpire(String key);
	
	/**
	 * 获取锁
	 * @param key
	 * @return
	 */
	public RLock getLock(String key);


}
