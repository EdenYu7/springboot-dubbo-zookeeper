package com.spring.test.provider.tack;

import lombok.extern.log4j.Log4j2;
import org.springframework.scheduling.annotation.Scheduled;


/**
 * @author: yuzh
 * @Description:
 * @date 2018/12/24
 */
@Log4j2
public class TestTack {


    @Scheduled(cron = "0 */30 *  * * * ")
//    @Scheduled(fixedRate = 6000)
    public void update() {

        log.info("半个小时执行一次");


    }

}
