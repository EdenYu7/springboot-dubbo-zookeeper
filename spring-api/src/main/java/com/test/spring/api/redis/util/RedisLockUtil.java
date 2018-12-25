package com.test.spring.api.redis.util;

import org.apache.commons.lang3.StringUtils;

public class RedisLockUtil {
	
	public static String getSyncIntegralLockName(String userOpenId) {
		if(StringUtils.isNotBlank(userOpenId)) {
			return "sync_integral"+ "_" + userOpenId;
		}
		return null;
	}

	public static String getSyncIntegralChangeLockName(String userOpenId) {
		if(StringUtils.isNotBlank(userOpenId)) {
			return "sync_integral_change"+ "_" + userOpenId;
		}
		return null;
	}

}
