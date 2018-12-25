package spring.test.common.enums;

/**
 * @author tangjie
 * description: api请求结果码枚举
 * date 2018/10/31 18:08
 */

public enum ResultCodeEnum {

    SUCCESS(0000, "请求成功"),
    UNKNOWN_ERROR(9999, "未知异常");

    ResultCodeEnum(int code, String msg) {
        this.code = code;
        this.msg = msg;
    }
    private int code;

    private String msg;

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }
}
