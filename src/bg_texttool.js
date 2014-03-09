chrome.storage.local.get(['enableTextTool', 'showKey', 'prefix'], function(items){
	"use strict";
	var context = CC['text'] = {};
	var $text = document.createElement('textarea'),
		enabled = items['enableTextTool'], // Boolean(localStorage.getItem('enableTextTool')),
		showKey = items['showKey'], // Boolean(localStorage.getItem('showKey')),
		prefixStr = items['prefix'] || '', // localStorage.getItem('prefix') || '',
		autoPrefix = true;
	var SPACE = '　　';

	
	var tMarkStr = '「」『』',
		sMarkStr = '“”‘’',
		tMarkReg = new RegExp('['+tMarkStr+']','g');
	var copyText = function(data){
		data = data.replace(tMarkReg, function(ch){
			return sMarkStr.charAt(tMarkStr.indexOf(ch)) || ch;
		}); //标点预处理
		var dataSet=data.match(/[^\n\r]+/g) || []; //断句, match比split快一点点
		for(var i=0,tmp;i<dataSet.length;){
			tmp=dataSet[i].replace(/[\s]+/g,' ').trim();
			if(tmp){
				dataSet[i]=tmp;
				i++;
			}else{
				dataSet.splice(i,1);
			}
		}
		if(dataSet.length===0){
			return;
		}
		var tdata = dataSet[0], ldata;
		if(dataSet.length===1 && getByteLength(tdata)<=50){//title directed
			var slen=(tdata.match(/\s/g) || []).length;
			if(slen>2 && tdata.length<slen*3){
				dataSet[0]=tdata.replace(/([^\w])\s/g,'$1').replace(/\s(?=[^\w])/g,'');//remove space
			}
		}else{//content directed
			if(!autoPrefix || dataSet.length===1){ //single part, no prefix
				dataSet[0]=SPACE+tdata;
			}else{
				if(tdata.substr(0, 5).indexOf('原标题') !== -1){
					//var m = tdata.replace(/原标题[:：\s]*/, '').match(/^[\(（\[\s]*(.*?)[\)）\]\s]*$/) || [];
					dataSet[0] = SPACE+'原标题：' + trimTitle(tdata);//m[1];
					dataSet[1] = prefixStr + dataSet[1];
				}else if((ldata = dataSet[dataSet.length - 1]).indexOf('原标题') !== -1){
					//var m = ldata.replace(/原标题[:：\s]*/, '').match(/^[\(（\[\s]*(.*?)[\)）\]\s]*$/) || [];
					dataSet.pop();
					dataSet.unshift(SPACE + '原标题：' + trimTitle(ldata));
					dataSet[1] = prefixStr + dataSet[1];
				}else{
					//do not add empty prefix
					//dataSet.unshift(SPACE+'原标题：');
					dataSet[0] = SPACE + prefixStr + tdata;
				}
			}
		}
		$text.value=dataSet.join('\n\n'+SPACE);
		$text.select();
		document.execCommand("Copy");
	};

	var tradStr = '錒皚噯藹靄愛嬡礙曖璦菴諳鵪銨闇晻翺鰲鼇襖媼嶴奧驁鈀壩罷鮁覇擺唄敗頒闆鈑辦絆幫綁謗鎊齙寶飽鴇報鮑盃桮鵯貝狽備揹鋇誖憊輩犇賁錛繃偪粃筆幣畢閉嗶蓽斃鉍篳痺蹕闢邊籩編鯿貶變緶辯辮標颮驃鏢飆飇鑣錶鰾鱉鼈別彆癟賓儐濱繽檳鑌瀕擯殯臏髕鬢氷餅稟並併竝撥剝缽鉢餑駁鈸鉑鵓鈽蔔補佈財埰寀採綵參葠蓡驂殘蠶慚慘燦倉傖滄蒼艙冊側厠廁惻測筞筴層挿鍤査詫釵儕蠆覘摻攙嬋讒禪饞纏蟬鐔產産諂剷鏟闡蕆囅懺顫倀閶鯧長腸萇嘗嚐償厰廠場悵暢鈔車硨徹塵陳諶磣闖襯稱齔趂櫬讖檉蟶鐺撐棖誠乗鋮懲騁喫鴟癡馳遲齒恥飭熾勅沖衝蟲寵銃儔幬綢疇籌躊讎讐醜齣芻廚耡鋤雛櫥躕礎儲処處絀觸傳釧囪瘡牀創愴搥錘鎚純脣蓴鶉綽輟齪詞辭鶿鷀餈賜從怱蓯樅蔥驄聰叢湊輳觕麤攛躥竄邨鹺銼錯噠達韃獃紿帶貸單擔鄲殫癉簞膽撣誕啗噉彈憚當儅噹襠擋黨讜氹碭蕩盪檔導島搗擣禱燾盜燈鄧櫈鐙隄鏑糴敵滌覿詆遞諦締蔕顛巔癲點電墊鈿澱彫鵰鯛弔釣調諜疊蜨鰈釘頂訂椗錠丟東鼕鶇動凍棟鬥鈄荳竇讀瀆櫝牘犢黷獨篤賭妬鍍斷緞鍛隊對兌懟鐓噸躉燉鈍頓遯奪鐸朶綞墮訛鋨鵝額阨軛堊惡噁餓諤閼齶鍔鶚顎鰐鱷兒鴯鮞爾邇餌鉺貳發髮罰罸閥琺繙礬釩煩氾汎飯範販鈁魴徬倣髣訪紡飛緋鯡誹廢費疿鐨紛雰墳奮僨憤糞鱝豐風灃楓瘋峯鋒馮縫諷鳳彿膚麩鳧紱紼輻嘸撫頫輔訃婦負駙複復賦縛鮒賻鰒該賅鈣蓋幹榦桿尷稈趕紺贛岡剛崗綱鋼槓戇臯縞鎬誥鋯紇肐鴿擱閣鎘個鉻給亙畊賡綆骾鯁宮龔鞏貢溝鈎鉤緱搆構詬購夠覯軲鴣轂鶻詁穀鈷蠱鵠皷顧僱錮鯝颳鴰剮詿掛枴柺関關觀鰥舘館筦貫慣摜鸛鑵廣獷歸媯龜規閨鮭軌匭詭劊劌櫃貴鱖袞緄輥滾鯀堝鍋蟈囯國幗摑菓槨過鉿駭頇韓漢釬銲頷絎頏蠔號暠皜顥灝訶閤閡覈頜闔賀鶴恆橫轟鬨紅閎葒鴻黌訌餱後鱟衚鬍壺鶘餬滸戶沍護滬芲華嘩譁驊鏵劃畫話樺懷壞懽歡還環鍰繯緩奐喚換渙煥瘓鯇黃鰉怳謊詼揮暉琿煇輝囘囬廻迴蚘痐匯彙滙會諱噦澮繪薈誨檜燴賄穢繢燬譭毀葷閽渾餛諢貨獲穫禍鑊譏擊嘰飢饑機璣磯雞鷄跡蹟積勣績緝齎躋齏羈級極輯幾蟣擠計記紀際劑嚌濟繼覬薊霽鱭鯽驥夾浹傢鎵郟莢鋏蛺頰賈鉀價駕戔姦堅殲間艱監牋箋緘縑鰹鶼韉揀梘儉繭撿筧減檢瞼襇鐧簡譾戩鹼見餞劍薦賤澗艦漸諫濺踐鑑鑒鍵檻薑將漿殭繮韁講獎槳蔣絳醬嬌澆驕膠鮫鷦僥撟絞餃矯腳鉸攪勦繳嶠轎較階癤稭節訐刦刧傑詰潔結頡鮚屆誡觔僅緊謹錦饉盡儘勁進藎晉燼贐縉覲涇經莖荊驚鯨剄頸淨弳徑逕脛痙競靚靜鏡糾鳩鬮韮舊鷲駒侷舉擧櫸齟詎鉅劇懼據颶鋸窶屨鵑鐫捲錈獧絹雋睠決訣絕覺譎軍鈞皸儁濬駿開凱剴塏愷鎧嘅鍇愾龕埳闞矙穅閌鈧攷銬軻鈳頦顆殼欬剋尅課騍緙錁墾懇鏗摳釦庫絝嚳褲誇塊儈鄶噲獪膾寬髖誆誑鄺壙纊況曠礦鑛貺虧巋窺闚匱憒媿潰蕢餽饋簣聵崐崑錕鯤綑閫睏擴闊臘蠟辢來崍徠淶萊錸賚睞賴瀨癩籟蘭嵐攔欄闌藍讕瀾襤斕籃鑭覽攬纜欖嬾懶爛濫瑯鋃蜋閬撈勞嘮嶗癆銠澇樂縲鐳誄壘淚類纍稜釐貍離驪犂鸝灕縭蘺鱺籬蔾禮裡裏邐鋰鯉鱧厤曆歷厲麗勵嚦壢瀝藶櫪癘隷隸儷櫟癧轢酈慄礪礫涖蒞糲蠣躒靂倆奩連簾憐漣蓮聯褳亷鰱鐮斂歛璉臉襝蘞練孌煉鍊戀殮鏈瀲涼樑糧兩魎諒輛遼療繚鐐鷯釕獵鄰隣臨轔鱗凜廩懍檁賃藺躪靈嶺淩鈴櫺欞綾蔆齡鯪領霤劉瀏畱餾騮鎦綹鷚龍嚨瀧蘢櫳瓏朧礱籠聾隴壟壠攏婁僂嘍蔞樓耬螻髏嶁摟簍瘺瘻鏤嚕擼盧廬蘆壚瀘爐鑪櫨臚轤鸕艫顱鱸鹵滷虜擄魯櫓艣陸錄賂輅淥祿濾轆鷺氌驢閭櫚呂侶鋁屢縷褸慮綠孿巒攣欒鸞臠灤鑾亂鋝掄侖崙倫圇淪綸輪論羅儸玀腡蘿邏欏鑼籮騾鏍臝濼絡犖駱媽嬤蔴馬獁瑪碼螞榪罵駡嘜嗎買蕒勱邁麥賣脈顢蠻饅瞞鰻滿蟎謾縵鏝貓氂犛錨鉚貿麼沒鶥黴鎂門捫鍆悶燜懣們懞濛矇錳夢彌瀰禰獼謎羋瞇覓祕冪謐綿緜黽緬靦麪麵鶓緲廟滅衊緡閔閩憫鳴銘謬繆謨饃糢歿驀鏌謀畝鉬幙拏挐內納鈉廼難柟撓鐃蟯惱腦鬧訥餒嫰鈮蜺鯢擬暱膩鮎鯰撚輦攆唸孃釀鳥蔦嫋裊嬝揑隉聶嚙齧囁鑷鎳顳躡寧嚀擰獰檸聹濘紐鈕農辳儂噥濃膿駑釹瘧煖儺諾謳歐毆甌鷗嘔慪漚盤蹣龐鉋砲皰賠轡噴鵬掽紕鈹毘羆駢諞騙縹飄貧嬪頻顰評憑蘋缾鮃釙潑頗僕撲鋪舖樸譜悽淒棲榿慼鏚齊臍頎騏騎蠐旂蘄鰭豈啓啟綺氣訖棄薺磧韆遷僉牽慳鉛謙簽籤騫蕁鈐錢鉗潛淺譴繾塹槧嗆戧槍蹌錆鏘鏹彊強墻牆嬙薔檣搶羥繈熗墝磽蹺鍬繰喬僑蕎橋譙誚竅翹竊愜篋鍥親欽懃鋟寢撳氫輕傾鯖檾頃請慶窮煢瓊坵鞦鰍虯賕巰區麯嶇詘敺驅軀趨鴝齲闃覰覷權詮輇銓踡顴綣勸卻愨確闋闕鵲搉羣讓蕘饒橈擾嬈繞熱認紉姙軔靭韌飪絨羢榮嶸蠑銣縟軟橤蘂銳叡閏潤灑颯薩鰓賽毿傘繖糝顙喪騷繅掃澀嗇銫穡殺紗鎩鯊篩曬刪姍釤羶閃陝訕騸繕饍贍鱔傷殤觴賞燒紹賒捨厙設慴懾攝灄紳詵審諗嬸瀋腎滲昇陞聲勝澠繩聖賸屍師蝨詩獅溼濕釃時識實蝕塒蒔鰣駛勢視試飾昰貰適軾鈰諡謚釋壽夀獸綬書紓樞儵攄輸贖藷術樹竪豎數潄帥閂雙誰稅順說説爍鑠碩絲噝鷥緦螄廝鍶飼駟鬆慫聳訟誦頌蒐餿颼鎪擻藪甦蘇囌穌訴肅謖泝痠雖綏隨嵗歲誶孫猻蓀損筍簑縮嗩瑣鎖牠鉈獺鰨撻闥駘臺颱檯擡鮐態鈦貪攤灘癱墰壇罈罎曇談錟譚襢鉭嘆歎湯鏜餳儻燙蹚濤絛縚縧搯韜鞀討騰謄籐銻綈緹鵜題體屜闐條齠鰷覜糶銚貼鐵廳聼聽烴鋌衕銅統慟頭禿圖凃塗釷團糰摶頹頽蛻飩託脫馱駝鴕鼉橢搨籜窪媧襪膃彎灣紈頑輓綰盌椀萬亾網輞朢為爲韋圍幃溈潙違闈潿維濰偉偽僞緯葦煒瑋諉韙鮪衛衞謂餵蝟溫紋聞閿穩問甕罋撾渦萵窩蝸臥齷烏汙汚鄔嗚誣鎢無吳蕪塢嫵廡憮鵡務誤騖霧鶩誒犧晳谿錫譆習蓆襲覡璽銑戯戲繫係餼細郤鬩潟蝦俠峽狹硤轄嚇廈僊縴纖秈薟躚鮮閒閑絃賢鹹嫺嫻啣銜癇鷳鷴顯險獫蜆蘚縣峴莧現綫線憲餡羨獻鄉薌廂緗驤鑲詳響餉饗鯗嚮項梟嘵驍綃蕭銷瀟簫囂曉篠効傚嘯蠍協脅脇挾諧擕攜擷纈寫洩瀉紲絏褻謝訢鋅釁興陘倖兇洶脩鵂饈綉繡銹鏽須鬚頊虛噓許詡敍敘卹勗緒續軒諼諠萲懸鏇選癬絢鉉學澩鱈謔勛勳塤燻尋馴詢潯鱘訓訊遜枒壓鴉椏鴨啞瘂亞訝埡婭氬嚥懨煙臙閹醃閆嚴喦巖鹽閻顏顔簷兗儼厴縯魘鼴厭彥硯艷豔騐驗諺燄鴈灩釅讞饜讌鷰贋贗鴦揚颺陽楊煬瘍養癢樣殀堯餚軺窯窰謠搖遙瑤鰩葯藥鷂燿爺埜業葉頁鄴曄燁謁靨毉醫銥儀詒迆飴貽迻遺頤彜釔艤螘蟻義億憶藝議異囈譯嶧懌繹詣驛軼誼縊瘞鎰鐿陰隂蔭廕慇銦婬銀齦飲隱癮應鶯嬰嚶攖纓甖罌櫻瓔鸚鷹塋滎熒瑩螢營縈瀅鎣瀠蠅贏潁穎癭暎喲傭擁癰雝鏞詠湧踴優憂猶郵蕕鈾遊魷銪祐誘紆餘歟魚娛諛漁踰覦輿與傴嶼俁語齬馭籲嫗飫鬱獄鈺預慾諭閾禦鵒瘉癒蕷譽鷸鳶鴛淵員園圓緣黿轅櫞遠願約嶽鑰悅鉞閱閲躍粵雲勻紜蕓鄖氳隕殞運鄆惲暈醖醞慍韞韻蘊雜災載偺攢趲暫賛贊讚鏨瓚贓駔髒臟蹧鑿棗竈皁譟則擇澤責嘖幘簀賾賊譖繒贈紥紮劄軋閘鍘詐柵搾齋債霑氈譫斬盞嶄輾佔戰棧綻張麞漲帳脹賬釗詔趙櫂輒蟄謫轍鍺這淛鷓貞針鍼偵湞楨禎診軫縝陣鴆賑鎮爭崢掙猙鉦睜錚箏証證諍鄭幀癥織梔執職縶蹠躑衹隻阯紙軹誌製幟質櫛摯緻贄輊擲鷙滯騭寘觶躓終鍾鐘腫種塚眾衆謅週軸紂縐晝皺驟硃誅諸豬銖櫧瀦櫫燭屬囑矚佇竚苧註貯駐築鑄專塼磚顓轉囀賺譔饌妝粧莊樁裝壯狀騅錐墜綴縋贅諄準濁諑鐲茲玆貲資緇諮輜錙齜鯔姉漬眥綜蹤縂總傯縱鄒騶諏鯫鏃詛組躦纘篹鉆鑽鱒',
		simpStr = '锕皑嗳蔼霭爱嫒碍暧瑷庵谙鹌铵暗暗翱鳌鳌袄媪岙奥骜钯坝罢鲅霸摆呗败颁板钣办绊帮绑谤镑龅宝饱鸨报鲍杯杯鹎贝狈备背钡悖惫辈奔贲锛绷逼秕笔币毕闭哔荜毙铋筚痹跸辟边笾编鳊贬变缏辩辫标飑骠镖飙飙镳表鳔鳖鳖别别瘪宾傧滨缤槟镔濒摈殡膑髌鬓冰饼禀并并并拨剥钵钵饽驳钹铂鹁钸卜补布财采采采彩参参参骖残蚕惭惨灿仓伧沧苍舱册侧厕厕恻测策䇲层插锸查诧钗侪虿觇掺搀婵谗禅馋缠蝉镡产产谄铲铲阐蒇冁忏颤伥阊鲳长肠苌尝尝偿厂厂场怅畅钞车砗彻尘陈谌碜闯衬称龀趁榇谶柽蛏铛撑枨诚乘铖惩骋吃鸱痴驰迟齿耻饬炽敕冲冲虫宠铳俦帱绸畴筹踌雠仇丑出刍厨锄锄雏橱蹰础储处处绌触传钏囱疮床创怆捶锤锤纯唇莼鹑绰辍龊词辞鹚鹚糍赐从匆苁枞葱骢聪丛凑辏粗粗撺蹿窜村鹾锉错哒达鞑呆绐带贷单担郸殚瘅箪胆掸诞啖啖弹惮当当当裆挡党谠凼砀荡荡档导岛捣捣祷焘盗灯邓凳镫堤镝籴敌涤觌诋递谛缔蒂颠巅癫点电垫钿淀雕雕鲷吊钓调谍叠蝶鲽钉顶订碇锭丢东冬鸫动冻栋斗钭豆窦读渎椟牍犊黩独笃赌妒镀断缎锻队对兑怼镦吨趸炖钝顿遁夺铎朵缍堕讹锇鹅额厄轭垩恶恶饿谔阏腭锷鹗颚鳄鳄儿鸸鲕尔迩饵铒贰发发罚罚阀珐翻矾钒烦泛泛饭范贩钫鲂彷仿仿访纺飞绯鲱诽废费痱镄纷氛坟奋偾愤粪鲼丰风沣枫疯峰锋冯缝讽凤佛肤麸凫绂绋辐呒抚俯辅讣妇负驸复复赋缚鲋赙鳆该赅钙盖干干杆尴秆赶绀赣冈刚岗纲钢杠戆皋缟镐诰锆纥胳鸽搁阁镉个铬给亘耕赓绠鲠鲠宫龚巩贡沟钩钩缑构构诟购够觏轱鸪毂鹘诂谷钴蛊鹄鼓顾雇锢鲴刮鸹剐诖挂拐拐关关观鳏馆馆管贯惯掼鹳罐广犷归妫龟规闺鲑轨匦诡刽刿柜贵鳜衮绲辊滚鲧埚锅蝈国国帼掴果椁过铪骇顸韩汉钎焊颔绗颃蚝号皓皓颢灏诃合阂核颌阖贺鹤恒横轰哄红闳荭鸿黉讧糇后鲎胡胡壶鹕糊浒户冱护沪花华哗哗骅铧划画话桦怀坏欢欢还环锾缳缓奂唤换涣焕痪鲩黄鳇恍谎诙挥晖珲辉辉回回迴回蛔蛔汇汇汇会讳哕浍绘荟诲桧烩贿秽缋毁毁毁荤阍浑馄诨货获获祸镬讥击叽饥饥机玑矶鸡鸡迹迹积积绩缉赍跻齑羁级极辑几虮挤计记纪际剂哜济继觊蓟霁鲚鲫骥夹浃家镓郏荚铗蛱颊贾钾价驾戋奸坚歼间艰监笺笺缄缣鲣鹣鞯拣枧俭茧捡笕减检睑裥锏简谫戬碱见饯剑荐贱涧舰渐谏溅践鉴鉴键槛姜将浆僵缰缰讲奖桨蒋绛酱娇浇骄胶鲛鹪侥挢绞饺矫脚铰搅剿缴峤轿较阶疖秸节讦劫劫杰诘洁结颉鲒届诫筋仅紧谨锦馑尽尽劲进荩晋烬赆缙觐泾经茎荆惊鲸刭颈净弪径径胫痉竞靓静镜纠鸠阄韭旧鹫驹局举举榉龃讵巨剧惧据飓锯窭屦鹃镌卷锩狷绢隽眷决诀绝觉谲军钧皲俊浚骏开凯剀垲恺铠慨锴忾龛坎阚瞰糠闶钪考铐轲钶颏颗壳咳克克课骒缂锞垦恳铿抠扣库绔喾裤夸块侩郐哙狯脍宽髋诓诳邝圹纩况旷矿矿贶亏岿窥窥匮愦愧溃蒉馈馈篑聩昆昆锟鲲捆阃困扩阔腊蜡辣来崃徕涞莱铼赉睐赖濑癞籁兰岚拦栏阑蓝谰澜褴斓篮镧览揽缆榄懒懒烂滥琅锒螂阆捞劳唠崂痨铑涝乐缧镭诔垒泪类累棱厘狸离骊犁鹂漓缡蓠鲡篱藜礼里里逦锂鲤鳢历历历厉丽励呖坜沥苈枥疠隶隶俪栎疬轹郦栗砺砾莅莅粝蛎跞雳俩奁连帘怜涟莲联裢廉鲢镰敛敛琏脸裣蔹练娈炼链恋殓链潋凉梁粮两魉谅辆辽疗缭镣鹩钌猎邻邻临辚鳞凛廪懔檩赁蔺躏灵岭凌铃棂棂绫菱龄鲮领溜刘浏留馏骝镏绺鹨龙咙泷茏栊珑胧砻笼聋陇垄垅拢娄偻喽蒌楼耧蝼髅嵝搂篓瘘瘘镂噜撸卢庐芦垆泸炉炉栌胪轳鸬舻颅鲈卤卤虏掳鲁橹橹陆录赂辂渌禄滤辘鹭氇驴闾榈吕侣铝屡缕褛虑绿孪峦挛栾鸾脔滦銮乱锊抡仑仑伦囵沦纶轮论罗㑩猡脶萝逻椤锣箩骡镙裸泺络荦骆妈嬷麻马犸玛码蚂杩骂骂唛吗买荬劢迈麦卖脉颟蛮馒瞒鳗满螨谩缦镘猫牦牦锚铆贸么没鹛霉镁门扪钔闷焖懑们蒙蒙蒙锰梦弥弥祢猕谜芈眯觅秘幂谧绵绵黾缅腼面面鹋缈庙灭蔑缗闵闽悯鸣铭谬缪谟馍模殁蓦镆谋亩钼幕拿拿内纳钠乃难楠挠铙蛲恼脑闹讷馁嫩铌霓鲵拟昵腻鲇鲶捻辇撵念娘酿鸟茑袅袅袅捏陧聂啮啮嗫镊镍颞蹑宁咛拧狞柠聍泞纽钮农农侬哝浓脓驽钕疟暖傩诺讴欧殴瓯鸥呕怄沤盘蹒庞刨炮疱赔辔喷鹏碰纰铍毗罴骈谝骗缥飘贫嫔频颦评凭苹瓶鲆钋泼颇仆扑铺铺朴谱凄凄栖桤戚戚齐脐颀骐骑蛴旗蕲鳍岂启启绮气讫弃荠碛千迁佥牵悭铅谦签签骞荨钤钱钳潜浅谴缱堑椠呛戗枪跄锖锵镪强强墙墙嫱蔷樯抢羟襁炝硗硗跷锹缲乔侨荞桥谯诮窍翘窃惬箧锲亲钦勤锓寝揿氢轻倾鲭苘顷请庆穷茕琼丘秋鳅虬赇巯区曲岖诎驱驱躯趋鸲龋阒觑觑权诠辁铨蜷颧绻劝却悫确阕阙鹊榷群让荛饶桡扰娆绕热认纫妊轫韧韧饪绒绒荣嵘蝾铷缛软蕊蕊锐睿闰润洒飒萨鳃赛毵伞伞糁颡丧骚缫扫涩啬铯穑杀纱铩鲨筛晒删姗钐膻闪陕讪骟缮膳赡鳝伤殇觞赏烧绍赊舍厍设慑慑摄滠绅诜审谂婶沈肾渗升升声胜渑绳圣剩尸师虱诗狮湿湿酾时识实蚀埘莳鲥驶势视试饰是贳适轼铈谥谥释寿寿兽绶书纾枢倏摅输赎薯术树竖竖数漱帅闩双谁税顺说说烁铄硕丝咝鸶缌蛳厮锶饲驷松怂耸讼诵颂搜馊飕锼擞薮苏苏苏稣诉肃谡溯酸虽绥随岁岁谇孙狲荪损笋蓑缩唢琐锁它铊獭鳎挞闼骀台台台抬鲐态钛贪摊滩瘫坛坛坛坛昙谈锬谭袒钽叹叹汤镗糖傥烫趟涛绦绦绦掏韬鼗讨腾誊藤锑绨缇鹈题体屉阗条龆鲦眺粜铫贴铁厅听听烃铤同铜统恸头秃图涂涂钍团团抟颓颓蜕饨托脱驮驼鸵鼍椭拓箨洼娲袜腽弯湾纨顽挽绾碗碗万亡网辋望为为韦围帏沩沩违闱涠维潍伟伪伪纬苇炜玮诿韪鲔卫卫谓喂猬温纹闻阌稳问瓮瓮挝涡莴窝蜗卧龌乌污污邬呜诬钨无吴芜坞妩庑怃鹉务误骛雾鹜诶牺晰溪锡嘻习席袭觋玺铣戏戏系系饩细却阋泻虾侠峡狭硖辖吓厦仙纤纤籼莶跹鲜闲闲弦贤咸娴娴衔衔痫鹇鹇显险猃蚬藓县岘苋现线线宪馅羡献乡芗厢缃骧镶详响饷飨鲞向项枭哓骁绡萧销潇箫嚣晓筱效效啸蝎协胁胁挟谐携携撷缬写泄泻绁绁亵谢欣锌衅兴陉幸凶汹修鸺馐绣绣锈锈须须顼虚嘘许诩叙叙恤勖绪续轩谖喧萱悬旋选癣绚铉学泶鳕谑勋勋埙熏寻驯询浔鲟训讯逊桠压鸦桠鸭哑痖亚讶垭娅氩咽恹烟胭阉腌阎严岩岩盐阎颜颜檐兖俨厣演魇鼹厌彦砚艳艳验验谚焰雁滟酽谳餍燕燕赝赝鸯扬扬阳杨炀疡养痒样夭尧肴轺窑窑谣摇遥瑶鳐药药鹞耀爷野业叶页邺晔烨谒靥医医铱仪诒迤饴贻移遗颐彝钇舣蚁蚁义亿忆艺议异呓译峄怿绎诣驿轶谊缢瘗镒镱阴阴荫荫殷铟淫银龈饮隐瘾应莺婴嘤撄缨罂罂樱璎鹦鹰茔荥荧莹萤营萦滢蓥潆蝇赢颍颖瘿映哟佣拥痈雍镛咏涌踊优忧犹邮莸铀游鱿铕佑诱纡余欤鱼娱谀渔逾觎舆与伛屿俣语龉驭吁妪饫郁狱钰预欲谕阈御鹆愈愈蓣誉鹬鸢鸳渊员园圆缘鼋辕橼远愿约岳钥悦钺阅阅跃粤云匀纭芸郧氲陨殒运郓恽晕酝酝愠韫韵蕴杂灾载咱攒趱暂赞赞赞錾瓒赃驵脏脏糟凿枣灶皂噪则择泽责啧帻箦赜贼谮缯赠扎扎札轧闸铡诈栅榨斋债沾毡谵斩盏崭辗占战栈绽张獐涨帐胀账钊诏赵棹辄蛰谪辙锗这浙鹧贞针针侦浈桢祯诊轸缜阵鸩赈镇争峥挣狰钲睁铮筝证证诤郑帧症织栀执职絷跖踯只只址纸轵志制帜质栉挚致贽轾掷鸷滞骘置觯踬终钟钟肿种冢众众诌周轴纣绉昼皱骤朱诛诸猪铢槠潴橥烛属嘱瞩伫伫苎注贮驻筑铸专砖砖颛转啭赚撰馔妆妆庄桩装壮状骓锥坠缀缒赘谆准浊诼镯兹兹赀资缁咨辎锱龇鲻姊渍眦综踪总总偬纵邹驺诹鲰镞诅组躜缵纂钻钻鳟',
		tradReg = new RegExp('['+tradStr+']','g');
	var testTrad = function(text){
		return tradReg.test(text);
	};
	var xhr=new XMLHttpRequest();
	xhr.onreadystatechange=function(){
		if(this.readyState===4){
			if(this.status===200){
				try{
					var result=JSON.parse(this.responseText.replace(/\u200b/g,'').replace(/,+/g,','))[0]; //damn the limitation of eval
					var data=[];
					for(var i=0,len=result.length;i<len;i++){
						data.push(result[i][0]);
					}
					copyText(data.join(''));
					//CC.showNotification('转换成功,可以粘贴了',2000);
				}catch(e){
					CC.showNotification('转换失败>﹏﹏<',2000);
				}
			}else{
				CC.showNotification('转换失败>﹏﹏<',2000);
			}
		}
	};
	var translateTrad = function(text){
		var localTrans = text.replace(tradReg,function(ch){
			return simpStr.charAt(tradStr.indexOf(ch)) || ch;
		});
		copyText(localTrans);
		xhr.open('POST','http://translate.google.cn/translate_a/t',true);
		xhr.setRequestHeader('Content-Type','application/x-www-form-urlencoded;charset=UTF-8');
		xhr.send('client=t&text='+encodeURIComponent(text)+'&hl=en&sl=zh-TW&tl=zh-CN&ie=UTF-8&oe=UTF-8&multires=1&otf=1&pc=1&ssel=0&tsel=4');
	};

	var getByteLength = function(str){
		return str.replace(/[^\u0000-\u00ff]/g,'xx').length;
	};

	var trimTitle = function(str){
		var title = str.split(/原标题[:：\s]*/)[1];
		if(!title){
			return '';
		}
		var left = title.match(/[\(\[\{（【｛]/g) || [],
			right = title.match(/[\)\]\}）】｝]/g) || [];
		if(right.length > left.length){
			title = title.replace(new RegExp('\\s*\\' + right.slice(left.length).join('[^\\)\\]\\}）】｝]*\\') + '[^\\)\\]\\}）】｝]*$'), '');
		}
		return title;
	}

	var cmdCopy = function(data,sender){
		var text=data;
		xhr.abort();
		autoPrefix = sender && sender.tab && !(/^file:\/\/\/.+\.txt$/.test(sender.tab.url)); //no prefix for local file or plugin
		if(testTrad(text)){
			//CC.showNotification('复制内容包括繁体字,转换中...',2000);
			translateTrad(text);
		}else{
			copyText(text);
		}
	}

	// context.onConfigChange = function(key, data){
	chrome.storage.onChanged.addListener(function(changes, area){
		if (area != 'local'){
			return;
		}
		var field;
		if(field = changes['prefix']){
			prefixStr = field.newVaule || '';
			// localStorage.setItem('prefix',data);
		}
		if(field = changes['showKey']){
			showKey = field.newVaule;
		}
		if(field = changes['enableTextTool']){
			enabled = field.newVaule;
			if(enabled){
				// localStorage.setItem('enableTextTool',1);
				CC.addCmd('copy', cmdCopy);
				chrome.browserAction.setIcon({path:{"19":"19bright.png", "38":"38bright.png"}});
			}else{
				// localStorage.removeItem('enableTextTool');
				CC.removeCmd('copy', cmdCopy);
				chrome.browserAction.setIcon({path:{"19":"19dark.png", "38":"38dark.png"}});
			}
		}
	});

	document.body.appendChild($text);
	if(enabled){
		CC.addCmd('copy', cmdCopy);
		chrome.browserAction.setIcon({path:{"19":"19bright.png", "38":"38bright.png"}});
	}
	CC.addCmd('ready', function(data,sender){
		if(enabled && showKey){
			chrome.tabs.executeScript(sender.tab.id,{
				file: './tool/showkeywords.js'
			});
		}
	});

});
