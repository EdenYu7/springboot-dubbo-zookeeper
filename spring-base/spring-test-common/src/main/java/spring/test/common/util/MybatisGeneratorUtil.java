package spring.test.common.util;

import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.generator.AutoGenerator;
import com.baomidou.mybatisplus.generator.config.DataSourceConfig;
import com.baomidou.mybatisplus.generator.config.GlobalConfig;
import com.baomidou.mybatisplus.generator.config.PackageConfig;
import com.baomidou.mybatisplus.generator.config.StrategyConfig;
import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;

import java.util.Map;


public class MybatisGeneratorUtil {

    /**
     * 根据模板生成generatorConfig.xml文件
     * @param map
     */
    public static void generator(Map<String, Object> map) {
        AutoGenerator mpg = new AutoGenerator();
        // 全局配置
        GlobalConfig gc = new GlobalConfig()
                // 作者
                .setAuthor((String) map.get("author"))
                // 输出目录
                .setOutputDir((String) map.get("outputDir"))
                // 是否覆盖文件
                .setFileOverride(true)
                // 开启 activeRecord 模式
                .setActiveRecord(false)
                // XML 二级缓存
                .setEnableCache(false)
                // XML ResultMap
                .setBaseColumnList(true)
                // XML columList
                .setBaseResultMap(true)
                .setServiceName("%sService");
                // 自定义文件命名，注意 %s 会自动填充表实体属性！
                // .setEntityName("%sEntity");
                // .setMapperName("%sDao")
                // .setXmlName("%sDao")
                // .setServiceName("MP%sService")
                // .setServiceImplName("%sServiceDiy")
                // .setControllerName("%sAction")
        mpg.setGlobalConfig(gc);

        // 数据源配置
        DataSourceConfig dsc = new DataSourceConfig()
                .setDbType(DbType.MYSQL)
                .setDriverName((String) map.get("jdbcDriver"))
                .setUsername((String) map.get("jdbcUsername"))
                .setPassword((String) map.get("jdbcPassword"))
                .setUrl((String) map.get("jdbcUrl"));
        mpg.setDataSource(dsc);

        // 策略配置
        StrategyConfig strategy = new StrategyConfig()
                // 表前缀
                .setTablePrefix((String) map.get("tablePrefix"))
                // 表名生成策略
                .setNaming(NamingStrategy.underline_to_camel)
                // 构建者模式
                .setEntityBuilderModel(true)

                .entityTableFieldAnnotationEnable(false);
        // 需要生成的表
        if (map.get("tableNames") != null) {
            strategy.setInclude((String[]) map.get("tableNames"));
        }
        mpg.setStrategy(strategy);

        // strategy.setCapitalMode(true);// 全局大写命名 ORACLE 注意
        // strategy.setExclude(new String[]{"test"}); // 排除生成的表
        // 自定义实体父类
        // strategy.setSuperEntityClass("com.baomidou.demo.TestEntity");
        // 自定义实体，公共字段
        // strategy.setSuperEntityColumns(new String[] { "test_id", "age" });
        // 自定义 mapper 父类
        // strategy.setSuperMapperClass("com.baomidou.demo.TestMapper");
        // 自定义 service 父类
        // strategy.setSuperServiceClass("com.baomidou.demo.TestService");
        // 自定义 service 实现类父类
        // strategy.setSuperServiceImplClass("com.baomidou.demo.TestServiceImpl");
        // 自定义 controller 父类
        // strategy.setSuperControllerClass("com.baomidou.demo.TestController");
        // 【实体】是否生成字段常量（默认 false）
        // public static final String ID = "test_id";
        // strategy.setEntityColumnConstant(true);
        // 【实体】是否为构建者模型（默认 false）
        // public User setName(String name) {this.name = name; return this;}
        // strategy.setEntityBuilderModel(true);

        // 自定义模板配置，可以 copy 源码 mybatis-plus/src/main/resources/templates 下面内容修改，
        // 放置自己项目的 src/main/resources/templates 目录下, 默认名称一下可以不配置，也可以自定义模板名称
        //TemplateConfig tc = new TemplateConfig();
        // tc.setController("...");
        //tc.setEntity("classpath:templates/entity.kt.vm");
        // tc.setMapper("...");
        // tc.setXml("...");
        // tc.setService("...");
        // tc.setServiceImpl("...");
        // 如上任何一个模块如果设置 空 OR Null 将不生成该模块。
        //mpg.setTemplate(tc);

        // 包配置
        PackageConfig pc = new PackageConfig()
                .setParent((String) map.get("packageName"))
                .setController("controller")
                .setEntity("dao.model.entity")
                .setMapper("dao.mapper")
                .setXml("dao.mapper.xml");
        mpg.setPackageInfo(pc);

        // 执行
        mpg.execute();
    }
}
