package com.test.spring.api.redis.lock;

import com.test.spring.api.service.RedissonClientService;
import com.test.spring.api.service.ServiceFactory;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.redisson.api.RLock;

import java.util.concurrent.TimeUnit;

/**
 * 分布式锁抽象
 * 
 * @author xuyongchen
 *
 */
@Slf4j
public abstract class AbstractBaseLock {
	
	private RedissonClientService redissonClientService =  ServiceFactory.getServiceStatement().redissonClientService;

	public String lockName;// 锁名称
	public int lockTimeout = 30;// 等待获取锁的超时时间，如果规定时间之内没有获取到锁，返回，单位：秒
	public int lockExpire;// 锁生命周期，超过这个时间，自动解锁。单位：秒

	public AbstractBaseLock() {
	}

	/**
	 * 尝试获取业务锁，并执行
	 */
	public boolean lockAndExecute() {
		// 过期时间，默认设为60s
		if (lockExpire <= 0) {
			lockExpire = 60;
		}
		if (StringUtils.isBlank(lockName))
			return false;
		long start = System.currentTimeMillis();
		String threadName = Thread.currentThread().getName();
		log.info("Thread=={}, lockName=={}, start get redis connection", threadName, lockName);
		// 创建锁
		RLock lock = redissonClientService.getLock(lockName);
		try {
			lock.expire(lockExpire, TimeUnit.SECONDS);
			log.info("Thread=={}, lockName=={}, start get lock, use time, user time=={}", threadName, lockName,
					(System.currentTimeMillis() - start));
			// 获取锁
			boolean result = lock.tryLock(lockTimeout, TimeUnit.SECONDS);
			if (result) {
				// 处理
				log.info("Thread=={}, lockName=={}, get lock and execute", threadName, lockName);
				return execute();
			} else {
				log.info("Thread=={}, lockName=={}, get lock failed", threadName, lockName);
				return false;
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		} finally {
			// 释放锁
			if (lock != null) {
				log.info("Thread=={}, lockName=={}, unlock", threadName, lockName);
				lock.unlock();
			}
		}
		log.info("Thread=={}, lockName=={}, unlock lock, total use time=={}", threadName, lockName,
				(System.currentTimeMillis() - start));
		return false;
	}

	/**
	 * 执行具体的业务
	 * 
	 * @return
	 */
	public abstract boolean execute();
}
