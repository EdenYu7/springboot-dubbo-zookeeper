package spring.test.common.util;

import org.apache.commons.lang3.StringUtils;

import java.io.UnsupportedEncodingException;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author yuzh
 * @since 2018-12-11
 */

public class StringUtil {
	public final static Random random = new Random();
	final static char[] hexChar = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
	final static char[] traditional = "皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃幣閉邊編貶變辯辮標鼈別癟瀕濱賓擯餅並撥缽鉑駁蔔補財參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵瀋陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊躥竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締顛點墊電澱釣調疊諜疊釘頂錠訂丟東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪墮鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛誹廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢號閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗將漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖鯨驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓冪綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒內擬膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜棲淒臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親寢輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲確讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽叁傘喪騷掃澀殺紗篩曬刪閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢適釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖隨綏歲孫損筍縮瑣鎖獺撻擡態攤貪癱灘壇譚談歎湯燙濤縧討騰謄銻題體屜條貼鐵廳聽烴銅統頭禿圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問甕撾蝸渦窩臥嗚鎢烏汙誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈嚇鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭囂銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許敘緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲隱櫻嬰鷹應纓瑩螢營熒蠅贏穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘柵詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙爭幀鄭證織職執紙摯擲幟質滯鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄著濁茲資漬蹤綜總縱鄒詛組鑽"
			.toCharArray();
	final static char[] simplified = "皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙币闭边编贬变辩辫标鳖别瘪濒滨宾摈饼并拨钵铂驳卜补财参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘沈陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑蹿窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔颠点垫电淀钓调迭谍叠钉顶锭订丢东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺堕鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞诽废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉号阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧将浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎鲸惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅幂绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁内拟腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱栖凄脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲寝轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊确让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛三伞丧骚扫涩杀纱筛晒删闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势适释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽随绥岁孙损笋缩琐锁獭挞抬态摊贪瘫滩坛谭谈叹汤烫涛绦讨腾誊锑题体屉条贴铁厅听烃铜统头秃图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝卧呜钨乌污诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦吓锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧嚣销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许叙绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮隐樱婴鹰应缨莹萤营荧蝇赢颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸栅诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰争帧郑证织职执纸挚掷帜质滞钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆着浊兹资渍踪综总纵邹诅组钻"
			.toCharArray();

	static {
		quickSort(traditional, simplified, 0, traditional.length - 1);
	}

	private static void quickSort(char ary1[], char ary2[], int low, int high) {
		if (low >= high) {
			return;
		}
		int first = low;
		int last = high;
		char key1 = ary1[low];
		char key2 = ary2[low];
		while (first < last) {
			while (last > first && ary1[last] >= key1)
				last--;
			if (first < last) {
				ary1[first] = ary1[last];
				ary2[first] = ary2[last];
				first++;
			}

			while (first < last && ary1[first] <= key1)
				first++;
			if (first < last) {
				ary1[last] = ary1[first];
				ary2[last] = ary2[first];
				last--;
			}
		}
		ary1[first] = key1;
		ary2[first] = key2;

		if (first + 1 < high) quickSort(ary1, ary2, first + 1, high);
		if (first - 1 > low) quickSort(ary1, ary2, low, first - 1);
	}

	public static String nullToString(int value) {
		return value == 0 ? "" : String.valueOf(value);
	}

	public static String nullToString(String value) {
		return (value == null ? "" : value);
	}

	public static String nullToString(String value, String encode) {
		if (value == null) return "";
		if (encode == null || encode.length() == 0) return value;
		try {
			return new String(value.getBytes("ISO-8859-1"), encode);
		} catch (UnsupportedEncodingException e) {
			return value;
		}
	}

	public static String emptyToString(String value, String defaultString) {
		if (value == null || value.trim().length() == 0) return defaultString;

		return value;
	}

	public static String left(String value, int len) {
		String rc = nullToString(value);
		if (len < 0) len = 0;

		return rc.length() > len ? rc.substring(0, len) : rc;
	}

	public static String right(String value, int len) {
		String rc = nullToString(value);
		if (len < 0) len = 0;

		return rc.length() > len ? rc.substring(rc.length() - len) : rc;
	}

	public static String paddingStringLeft(String s, int width, char ch) {
		if (s == null) s = "";
		for (int i = s.length(); i < width; i++)
			s = ch + s;

		return s;
	}

	public static String paddingStringRight(String s, int width, char ch) {
		if (s == null) s = "";
		for (int i = s.length(); i < width; i++)
			s = s + ch;

		return s;
	}

	public static short stringToShort(String strNumber) {
		return stringToShort(strNumber, (short) 0);
	}

	public static short stringToShort(String strNumber, short defaultValue) {
		short result = defaultValue;
		try {
			if (strNumber != null && strNumber.length() > 0) result = Short.parseShort(strNumber);
		} catch (NumberFormatException e) {
		}
		return result;
	}

	public static int stringToInt(String strNumber) {
		return stringToInt(strNumber, 0);
	}

	public static int stringToInt(String strNumber, int defaultValue) {
		int result = defaultValue;
		try {
			if (strNumber != null && strNumber.length() > 0) result = Integer.parseInt(strNumber);
		} catch (NumberFormatException e) {
		}
		return result;
	}

	public static long stringToLong(String strNumber) {
		return stringToLong(strNumber, 0);
	}

	public static long stringToLong(String strNumber, long defaultValue) {
		long result = defaultValue;
		try {
			if (strNumber != null && strNumber.length() > 0) result = Long.parseLong(strNumber);
		} catch (NumberFormatException e) {
		}
		return result;
	}

	public static double stringToDouble(String strNumber) {
		return stringToDouble(strNumber, 0.00);
	}

	public static double stringToDouble(String strNumber, double defaultValue) {
		double result = defaultValue;
		try {
			if (strNumber != null && strNumber.length() > 0) result = Double.parseDouble(strNumber);
		} catch (NumberFormatException e) {
		}
		return result;
	}

	public static Date stringToDate(String strDate) {
		return stringToDate(strDate, new Date(0));
	}

	public static Date stringToDate(String strDate, Date defaultValue) {
		Date date = defaultValue;
		try {
			if (strDate != null && strDate.length() > 0) date = java.sql.Date.valueOf(strDate);
		} catch (IllegalArgumentException e) {
		}
		return date;
	}

	public static java.sql.Date stringToSqlDate(String strDate) {
		return stringToSqlDate(strDate, null);
	}

	public static java.sql.Date stringToSqlDate(String strDate, java.sql.Date defaultValue) {
		java.sql.Date date = defaultValue;
		try {
			if (strDate != null && strDate.length() > 0) date = java.sql.Date.valueOf(strDate);
		} catch (IllegalArgumentException e) {
		}
		return date;
	}

	public static Time stringToSqlTime(String strTime) {
		return stringToSqlTime(strTime, null);
	}

	public static Time stringToSqlTime(String strTime, Time defaultValue) {
		Time time = defaultValue;
		try {
			if (strTime != null && strTime.length() > 0) {
				if (strTime.indexOf(':') == strTime.lastIndexOf(':')) strTime += ":00";
				time = Time.valueOf(strTime);
			}
		} catch (IllegalArgumentException e) {
		}
		return time;
	}

	public static Timestamp stringToTimestamp(String strTime) {
		return stringToTimestamp(strTime, new Timestamp(0));
	}

	public static Timestamp stringToTimestamp(String strTime, Timestamp defaultValue) {
		Timestamp time = defaultValue;
		try {
			if (strTime != null && strTime.length() > 0) time = Timestamp.valueOf(strTime);
		} catch (IllegalArgumentException e) {
		}
		return time;
	}

	public static boolean isNumber(String s) {
		if (s == null || s.length() == 0) return false;
		byte b[] = s.getBytes();
		for (int i = 0; i < b.length; i++) {
			if (b[i] < '0' || b[i] > '9') return false;
		}

		return true;
	}

	public static String shortString(String value, int len) {
		String rc = nullToString(value);
		if (rc.length() <= len && len > 0) return rc;

		return value.substring(0, len) + "...";
	}

	public static String shortAnsiString(String value, int len) {
		String rc = nullToString(value);
		if (rc.getBytes().length > len) {
			int trimLen = (rc.getBytes().length - len + 1) / 2;
			int strLen = rc.length();
			while (rc.substring(0, strLen - trimLen).getBytes().length > len) {
				trimLen++;
			}
			rc = value.substring(0, strLen - trimLen) + "...";
		}

		return rc;
	}

	public static String formatCurrency(double value) {
		DecimalFormat df = new DecimalFormat("0.00");
		return df.format(value);
	}

	public static String setSentencePeriod(String value) {
		if (value == null || value.trim().length() == 0) {
			return "";
		}

		if (!value.matches(".*[,.，。]$")) {
			value += "。";
		}

		return value;
	}

	public static boolean isEmpty(String value) {
		if (value == null || value.trim().length() == 0) {
			return true;
		}
		return false;
	}

	public static String getRandomString(int length) {
		StringBuffer sb = new StringBuffer(length);
		for (int i = 0; i < length; i++) {
			sb.append(hexChar[random.nextInt(16)]);

		}
		return sb.toString();
	}

	public static String getRandomNumber(int length) {
		StringBuffer sb = new StringBuffer(length);
		for (int i = 0; i < length; i++) {
			sb.append(random.nextInt(10));
		}
		return sb.toString();
	}

	public static String getSafeString(String value) {
		return value == null ? null : value.replaceAll("'", "\\\\'");
	}

	public static byte[] hex2Byte(String str) {
		byte[] rc = null;
		if (str == null || str.length() == 0) return rc;
		int len = str.length() / 2;
		rc = new byte[len];
		for (int i = 0; i < len; i++) {
			try {
				rc[i] = (byte) (Integer.parseInt(str.substring(2 * i, 2 * i + 2), 16) & 0XFF);
			} catch (NumberFormatException e) {
			}
		}

		return rc;
	}

	public static String byte2Hex(byte b[]) {
		if (b == null) return "";
		StringBuffer tmp = new StringBuffer();
		int len = b.length;
		for (int i = 0; i < len; i++) {
			tmp.append(hexChar[(b[i] >>> 4) & 0x0F]);
			tmp.append(hexChar[b[i] & 0x0F]);
		}

		return tmp.toString();
	}

	public static float stringSimilarity(String str1, String str2) {
		// 计算两个字符串的长度。
		int len1 = str1.length();
		int len2 = str2.length();
		// 建立上面说的数组，比字符长度大一个空间
		int[][] dif = new int[len1 + 1][len2 + 1];
		// 赋初值，步骤B。
		for (int a = 0; a <= len1; a++) {
			dif[a][0] = a;
		}
		for (int a = 0; a <= len2; a++) {
			dif[0][a] = a;
		}
		// 计算两个字符是否一样，计算左上的值
		int temp;
		for (int i = 1; i <= len1; i++) {
			for (int j = 1; j <= len2; j++) {
				if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
					temp = 0;
				} else {
					temp = 1;
				}
				// 取三个值中最小的
				dif[i][j] = min(dif[i - 1][j - 1] + temp, dif[i][j - 1] + 1, dif[i - 1][j] + 1);
			}
		}

		return 1 - (float) dif[len1][len2] / Math.max(len1, len2);
	}

	// 得到最小值
	private static int min(int... is) {
		int min = Integer.MAX_VALUE;
		for (int i : is) {
			if (min > i) {
				min = i;
			}
		}
		return min;
	}

	public static String traditionalToSimplified(String s) {
		if (s == null || s.length() == 0) {
			return s;
		}
		char[] buff = s.toCharArray();
		for (int i = 0; i < buff.length; i++) {
			int pos = Arrays.binarySearch(traditional, buff[i]);
			if (pos >= 0) {
				buff[i] = simplified[pos];
			}
		}

		return new String(buff);
	}

	public static String camel2Dash(String value) {
		if (value == null || value.length() == 0) {
			return value;
		}
		StringBuffer sb = new StringBuffer();
		for (char c : value.toCharArray()) {
			if (c >= 'A' && c <= 'Z') {
				sb.append('_');
				sb.append((char) (c + 0x20));
			} else {
				sb.append(c);
			}
		}

		return sb.toString();
	}

	public static String dash2Camel(String value) {
		if (value == null || value.length() == 0) {
			return value;
		}
		StringBuffer sb = new StringBuffer();
		char[] chars = value.toCharArray();
		for (int i = 0; i < chars.length; i++) {
			if (chars[i] == '_' && i < chars.length && chars[i + 1] >= 'a' && chars[i + 1] <= 'z') {
				sb.append((char) (chars[++i] - 0x20));
			} else {
				sb.append(chars[i]);
			}
		}

		return sb.toString();
	}

	public static List<Integer> splitStringList(String str){
		return splitStringList(str, ",");
	}
	public static List<Integer> splitStringList(String str,String spitStr){
		if(StringUtils.isBlank(str))
			return new ArrayList<Integer>();
		String[] ss = str.split(spitStr);
		try {
			List<Integer> list = new ArrayList<Integer>();
			for(String s : ss){
                list.add(Integer.parseInt(s));
            }
			return list;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return new ArrayList<Integer>();
	}
	public static List<Integer> splitStringListNotZeroRepeat(String str,String spitStr){
		if(StringUtils.isBlank(str))
			return new ArrayList<Integer>();
		String[] ss = str.split(spitStr);
		try {
			List<Integer> list = new ArrayList<Integer>();
			for(String s : ss){
				int num = Integer.parseInt(s);
				if(num <= 0 || list.contains(num))
					continue;
                list.add(num);
            }
			return list;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return new ArrayList<Integer>();
	}

	/**
	 * 将xx,yy,zz字符串拆分成集合   去重
	 * @param tags 字符串
	 * @return 集合
	 */
	public static List<String> splitTags(String tags, String separator) {

		if (StringUtils.isBlank(tags)) {
			return null;
		}
		String[] feedStrs = StringUtils.split(tags, separator);
		List<String> result = null;
		if (feedStrs != null && feedStrs.length > 0) {
			result = new ArrayList<String>();
			for (int i = 0;i < feedStrs.length ; i++) {
				String feedIdStr = feedStrs[i];
				if (!result.contains(feedIdStr) && StringUtils.isNotBlank(feedIdStr)) {
					result.add(feedIdStr);
				}

			}
		}
		return result;
	}

	/**
	 * 手机号验证
	 */
	public static boolean isMobile(String str) {
		Pattern p = null;
		Matcher m = null;
		boolean b = false;
		p = Pattern.compile("^[1][3,4,5,7,8][0-9]{9}$"); // 验证手机号
		m = p.matcher(str);
		b = m.matches();
		return b;
	}

	/**
	 * 该方法匹配字符串中是否有包含emoji表情
	 * @param source
	 * @return
	 */
	public static boolean matcherEmoji(String source) {
		if(StringUtils.isBlank(source)){
			return false;
		}
		Pattern emoji = Pattern.compile("[\ud83c\udc00-\ud83c\udfff]|[\ud83d\udc00-\ud83d\udfff]|[\u2600-\u27ff]", Pattern.UNICODE_CASE | Pattern.CASE_INSENSITIVE);
		Matcher emojiMatcher = emoji.matcher(source);
		if (emojiMatcher.find()) {
//            source = emojiMatcher.replaceAll("*");
			return true;
		}
		return false;
	}




}
