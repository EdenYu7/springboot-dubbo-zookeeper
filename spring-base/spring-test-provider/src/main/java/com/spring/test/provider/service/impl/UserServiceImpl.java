package com.spring.test.provider.service.impl;


import com.alibaba.dubbo.config.annotation.Service;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.spring.base.dao.mapper.UserMapper;
import com.spring.base.dao.model.entity.User;
import com.spring.test.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author yuzh
 * @since 2018-12-24
 */
@Service
public class UserServiceImpl implements UserService {

    @Autowired
    UserMapper userMapper ;

    @Override
    public String getUserNameByUserId(int userId) {

        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("userId",userId);

        User user = userMapper.selectOne(queryWrapper);

        return user.getUserName();
    }
}
