package com.test.spring.api.crontroller.base;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.bind.annotation.ExceptionHandler;
import spring.test.common.enums.ResultCodeEnum;
import spring.test.common.result.BaseResult;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;


@Slf4j
public abstract class BaseController {

    @ExceptionHandler
    public Object exceptionHandler(HttpServletRequest request, HttpServletResponse response, Exception exception){
        log.error("统一处理异常:{}", exception);
        request.setAttribute("ex", exception);
        log.info(exception.getClass().getName());
        return new BaseResult(ResultCodeEnum.UNKNOWN_ERROR.getCode(), exception.getMessage());
    }


    /**
     * 把所有参数有效转成map
     */
    private Map getParameterMap(HttpServletRequest request){

        Map map = new HashMap<>();
        Enumeration<String> keys = request.getParameterNames();
        while(keys.hasMoreElements()){
            String key = keys.nextElement();
            if (StringUtils.isNotEmpty(request.getParameter(key))){
                map.put(key,request.getParameter(key));
            }
        }
        return map ;

    }



}
