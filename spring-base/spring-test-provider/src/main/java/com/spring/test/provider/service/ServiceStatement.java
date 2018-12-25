package com.spring.test.provider.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 管理service
 *
 */
@Component("serviceStatement")
public class ServiceStatement {

    @Autowired
    public RedissonClientService redissonClientService ;




    /**
     * 无参数的构造函数
     */
    public ServiceStatement() {

    }
}
