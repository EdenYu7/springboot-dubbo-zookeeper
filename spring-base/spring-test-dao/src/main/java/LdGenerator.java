import org.apache.commons.lang3.StringUtils;
import spring.test.common.util.MybatisGeneratorUtil;
import spring.test.common.util.PropertiesFileUtil;

import java.util.HashMap;
import java.util.Map;

public class LdGenerator {

    /**
     * 表前缀
     */
    private static String TABLE_PREFIX = PropertiesFileUtil.getInstance("generator").get("table_prefix");
    /**
     * author
     */
    private static String AUTHOR = PropertiesFileUtil.getInstance("generator").get("author");
    /**
     * 包路径
     */
    private static String PACKAGE_NAME = PropertiesFileUtil.getInstance("generator").get("package");
    /**
     * 输出路径
     */
    private static String OUTPUT_DIR = PropertiesFileUtil.getInstance("generator").get("outputdir");
    /**
     * mysql-jdbc parameters
     */
    private static String JDBC_DRIVER = PropertiesFileUtil.getInstance("generator").get("jdbc.driver");
    private static String JDBC_URL = PropertiesFileUtil.getInstance("generator").get("jdbc.url");
    private static String JDBC_USERNAME = PropertiesFileUtil.getInstance("generator").get("jdbc.username");
    private static String JDBC_PASSWORD = PropertiesFileUtil.getInstance("generator").get("jdbc.password");
    /**
     * tableNames
     */
    private static String TABLE_NAMES = PropertiesFileUtil.getInstance("generator").get("table.names");

    /**
     * 自动代码生成
     * @param args
     */
    public static void main(String[] args) {
        Map<String, Object> map = initParam();
        MybatisGeneratorUtil.generator(map);
    }

    public static Map<String, Object> initParam() {
        Map<String, Object> map = new HashMap<>(16);
        map.put("author", AUTHOR);
        map.put("jdbcDriver", JDBC_DRIVER);
        map.put("jdbcUrl", JDBC_URL);
        map.put("jdbcUsername", JDBC_USERNAME);
        map.put("jdbcPassword", JDBC_PASSWORD);
        map.put("outputDir", OUTPUT_DIR);
        map.put("tablePrefix", TABLE_PREFIX);
        map.put("packageName", PACKAGE_NAME);
        if (StringUtils.isNotBlank(TABLE_NAMES)) {
            map.put("tableNames", TABLE_NAMES.split(","));
        }
        return map;
    }
}
