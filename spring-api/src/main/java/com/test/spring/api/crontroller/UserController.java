package com.test.spring.api.crontroller;


import com.alibaba.dubbo.config.annotation.Reference;
import com.spring.test.service.UserService;
import com.test.spring.api.service.ServiceFactory;
import io.swagger.annotations.ApiOperation;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import spring.test.common.util.StringUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author yuzh
 * @since 2018-12-24
 */
@Controller
@RequestMapping("/user")
public class UserController {

    public UserService userService = ServiceFactory.getServiceStatement().userService;

    @ApiOperation(value="获取测试数据")
    @GetMapping("/test")
    public Object getPage(HttpServletRequest request, HttpServletResponse response){

        int userId = StringUtil.stringToInt(request.getParameter("userId"),0);

        String userName = userService.getUserNameByUserId(userId);

        return userName ;
    }
}

