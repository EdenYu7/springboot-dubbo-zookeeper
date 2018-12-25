package com.test.spring.api.service;


import com.test.spring.api.ApplicationContextBean;
import org.springframework.context.ApplicationContext;

public class ServiceFactory {

    private static ServiceStatement serviceStatement;

    public static synchronized ServiceStatement getServiceStatement(){

        if(null == serviceStatement){
            ApplicationContext context = ApplicationContextBean.getApplicationContext();
            System.out.println("获取================> serviceStatement");
            serviceStatement = (ServiceStatement)context.getBean("serviceStatement");
        }
        return serviceStatement;
    }



}
