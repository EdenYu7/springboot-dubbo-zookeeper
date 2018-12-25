package spring.test.common.result;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import spring.test.common.enums.ResultCodeEnum;

@ApiModel(description="返回结果")
@Data
public class BaseResult {

    /**
     * 状态码：1成功，其他为失败
     */
    @ApiModelProperty(value = "返回结果,code=0表示成功")
    private int errcode;

    /**
     * 成功为OK，其他为失败原因
     */
    @ApiModelProperty(value = "结果信息,成功返回OK,错误会返回对应的错误原因")
    public String errmsg;

    /**
     * 数据结果集
     */
    @ApiModelProperty(value = "接口所需的业务数据")
    public Object data;

    public BaseResult(int errcode, String errmsg) {
        this.errcode = errcode;
        this.errmsg = errmsg;
    }

    public BaseResult(int errcode, String errmsg, Object data) {
        this.errcode = errcode;
        this.errmsg = errmsg;
        this.data = data;
    }

}
