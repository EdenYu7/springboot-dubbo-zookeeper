package com.test.spring.api.service;

import com.alibaba.dubbo.config.annotation.Reference;
import com.spring.test.service.UserService;
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

    @Reference
    public UserService userService ;




    /**
     * 无参数的构造函数
     */
    public ServiceStatement() {

    }
}
