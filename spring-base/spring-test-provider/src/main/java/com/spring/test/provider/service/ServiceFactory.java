package com.spring.test.provider.service;


import com.spring.test.provider.ApplicationContextBean;
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
