var el=document.getElementById('text');
var stage=document.getElementById('stage');
function copyText(data){
	var dataSet=data.match(/[^\n\r]+/g);
	for(var i=0,tmp;i<dataSet.length;){
		tmp=dataSet[i].replace(/[\s]+/g,' ').trim();
		if(tmp){
			dataSet[i]='　　'+tmp;
			i++;
		}else{
			dataSet.splice(i,1);
		}
	}
	if(dataSet.length==1 && getByteLength(dataSet[0])<=54){//title directed
		var tdata=dataSet[0].substr(2),
			slen=(tdata.match(/\s/g) || []).length;
		if(slen>2 && tdata.length<slen*3){
			tdata=tdata.replace(/([^\w])\s/g,'$1').replace(/\s(?=[^\w])/g,'');//remove space
		}
		dataSet[0]=tdata;
	}
	el.value=dataSet.join('\n\n');
	el.select();
	document.execCommand("Copy");
}

var tradReg=/[錒鎄皚噯藹靄愛嬡礙曖璦菴諳鵪鞌垵銨闇晻翶翺鰲鼇襖媼嶴奧驁鈀垻壩罷鮁覇擺唄敗粺頒岅闆鈑辦絆幫綁牓艕謗鎊齙裦寶飽鴇緥報鮑盃桮鵯貝狽備揹鋇誖憊輩韝逩犇賁錛綳繃偪粃筆幣畢閉嗶蓽斃鉍篳潷痺蹕闢獘邊籩編鯿貶變緶辯辮標颮驃臕鏢飆飇飈鑣錶鰾鱉鼈別彆癟賓賔儐濱繽檳鑌瀕擯殯臏髕髩鬢氷餅稟並併竝撥剝缽鉢餑駁駮鈸鉑愽鵓鈽蔔補佈鈈財埰寀採綵倸跴參葠蓡驂殘蠶慚慙慘黲燦倉傖滄蒼艙撡艸冊側厠廁惻測筞筴層挿餷鍤査詧鑔詫釵儕蠆覘摻攙嬋讒禪饞纏蟬鐔產産諂剷鏟闡蕆囅懺顫倀閶鯧長腸萇嘗嚐償厰廠塲場悵暢鈔車硨撦徹塵陳訦諶硶磣闖襯稱齔趂櫬讖檉蟶鐺撐棖誠乗鋮懲堘澂騁喫鴟癡馳遲齒恥飭熾勅沖衝蟲寵銃儔幬綢疇籌詶酧醻躊讎讐醜矁齣芻廚耡鋤雛櫥躕礎儲処處絀觸傳舩釧囪瘡窓牎牕牀創愴搥箠錘鎚旾純脣蒓蓴湻鶉醕綽輟齪詞辤辭鶿鷀餈賜從怱悤蓯樅蔥驄聰樷叢湊輳觕麤蹵攛鑹躥竄簒脃邨鹺銼錯噠達遝韃獃紿帶蝳貸單擔鄲殫癉簞膽撣誕啗噉彈憚當儅噹襠擋攩黨讜氹碭蕩盪檔導島搗擣禱燾盜鍀悳燈鄧櫈鐙隄鏑糴敵滌覿詆牴觝遞諦締蔕顛巔癲點電墊鈿澱彫琱鵰鯛弔釣調銱諜啑曡疉疊蜨鰈釘頂訂矴椗錠丟銩東鼕崬崠鶇動凍峝棟腖兠鬥鬦鬭鈄荳竇讀凟瀆匵櫝牘犢黷獨篤賭覩妬鍍耑斷緞煆鍛籪隊對兌懟鐓噸墪躉燉鈍頓遯奪鐸朶垜綞墮跥訛譌峩鋨鵝鵞額娿戹阨軛堊惡噁餓諤閼蕚齶鍔鶚顎鰐鱷兒鴯鮞爾邇餌鉺貳發髮罰罸閥灋琺颿飜繙凣礬釩煩緐氾汎飯範販鈁魴徬倣髣訪紡飛緋鯡誹廢費疿鐨紛雰墳奮僨憤糞鱝豐風灃楓瘋碸峯鋒馮縫諷鳳彿伕膚麩粰鳧紱紼輻襆嘸撫俛頫輔訃婦負坿駙複復賦縛鮒賻鰒釓嘠該賅匃匄鈣蓋槩幹榦桿尲尷稈趕紺贛岡剛崗綱疘鋼槓戇臯橰餻縞槀鎬誥鋯紇肐鴿擱謌閣鎘個箇鉻給亙畊賡綆骾鯁宮躳龔鞏貢溝鈎鉤緱搆構詬購夠覯軲鴣轂鶻詁穀鈷蠱鵠皷顧僱錮鯝颳鴰剮詿掛枴柺恠関關觀鰥舘館琯筦貫慣摜鸛鑵廣獷歸媯嬀龜規槼閨瓌鮭軌匭詭劊劌櫃貴鱖袞緄輥滾鮌鯀咼堝鍋蟈囯國幗摑菓槨過鉿駭頇圅韓漢猂釬銲頷絎頏蠔獋號暠皜顥灝訶閤郃咊閡覈盇頜闔賀鶴恆橫轟鬨紅閎葒鴻黌訌餱鱟虖嘑謼軤衚鬍壺鶘餬滸戶沍護滬鸌芲蘤華嘩譁驊鏵劃畫話樺懷壞懽歡貛還環鍰繯緩奐喚換渙煥瘓鯇黃鰉怳謊詼噅揮暉琿煇輝幑囘囬廻迴蚘痐蛕蜖匯彙滙會諱噦澮繪薈誨檜燴賄穢繢燬譭毀昬葷閽渾餛諢鍃鈥貨獲穫禍鑊譏擊嘰飢饑機璣磯雞鷄跡蹟積勣績緝賫齎躋齏羈級極檝輯幾蟣擠計記紀際劑嚌濟繼覬薊霽鱭鯽驥夾裌浹傢鎵郟莢鋏蛺頰賈鉀價駕戔姦堅殲間艱監牋箋緘縑鰹鶼韉揀梘儉繭撿筧減檢瞼襇鐧簡譾戩鹼堿見餞劍劒薦賤澗艦漸諫濺踐鋻鑑鑒鍵檻薑將漿殭繮韁講獎槳蔣絳醬嬌澆驕膠鮫鷦僥撟絞餃矯腳鉸攪勦繳呌嶠轎較階堦癤稭節訐刦刧刼傑詰潔結頡鮚屆誡觔僅巹緊謹錦饉盡儘勁進藎晉燼賮贐縉覲涇經莖荊驚鯨穽剄頸淨弳徑逕脛痙競靚靜鏡逈埛糾鳩鬮揫韮舊廄廐捄鷲駒鋦侷跼舉擧櫸齟詎鉅劇懼據颶鋸窶屨鵑鎸鐫捲錈勌棬獧絹雋睠決訣玨絕覺譎橜鐝钁軍鈞皸儁濬駿哢開鐦凱剴塏愷鎧嘅鍇愾龕埳偘闞矙粇穅閌匟鈧攷銬軻痾鈳頦顆殼欬剋尅課騍緙錁肎墾懇阬鏗摳瞘敂釦宼庫絝嚳褲誇塊儈鄶噲獪膾寬髖欵誆誑鄺壙纊況曠礦鑛貺虧巋窺闚匱憒媿潰蕢餽饋簣聵堃崐崑錕鯤梱綑閫睏擴濶闊臘蠟辢來崍徠淶萊錸賚睞賴顂瀨癩籟蘭嵐攔欄惏闌藍讕瀾襤斕籃鑭覽攬纜欖嬾懶爛濫瑯鋃蜋閬撈勞嘮嶗癆鐒銠澇耮樂鰳縲鐳誄壘淚類纍稜釐棃貍離驪犂鸝灕縭蘺琍瓈鱺籬蔾禮裡裏邐鋰鯉鱧厤曆歷厲麗勵嚦壢瀝藶櫪癘隷隸儷櫟鬁癧茘轢酈慄礪礫涖蒞糲蠣躒靂倆匲奩匳籢連簾憐漣蓮聯褳亷鰱鐮斂歛璉臉襝蘞練孌煉鍊戀殮鏈瀲涼樑糧兩魎諒輛遼療繚鐐鷯釕獵鄰隣臨痳轔粦燐鱗麐凜廩懍檁恡賃藺躪霛靈嶺淩鈴櫺欞綾蔆齡鯪領霤劉瀏畱瑠璢餾騮癅鎦栁桺綹鋶鷚龍嚨瀧蘢櫳瓏朧礱籠聾隴壟壠攏婁僂嘍蔞樓耬螻髏嶁摟簍瘺瘻鏤嚕擼盧廬蘆壚罏瀘爐鑪櫨臚轤鸕艫顱鱸鹵滷虜擄魯櫓艣艪鑥陸錄賂輅淥祿濾剹轆鷺氌驢閭櫚呂侶穭鋁屢縷褸慮綠孿巒攣欒鸞臠灤鑾亂畧鋝掄侖崙倫圇淪綸輪論羅儸玀腡蘿邏欏鑼籮騾驘鏍躶臝濼絡犖駱媽嬤蔴蟇馬獁瑪碼螞榪罵駡嘜嗎買蕒勱邁麥賣脈衇顢蠻饅瞞鰻滿蟎謾縵鏝貓氂犛錨鉚冐貿夘戼麼沒楳槑鎇鶥黴鎂門捫鍆悶燜懣們懞濛矇錳夢彌瀰禰獼謎羋瞇覓覔祕冪謐綿緜黽緬靦靣麪麵鶓緲玅廟哶滅衊瑉緍緡閔冺閩憫湣鰵鳴銘謬繆謨饃饝糢歿驀鏌謀畝鉬幙拏挐鎿內納鈉廼迺嬭難枏柟饢撓鐃蟯堖惱腦閙鬧訥餒嫰鈮蜺鯢妳擬暱膩鮎鯰撚輦攆唸孃釀鳥蔦嫋裊嬝揑隉聶嚙齧囁鑷鎳顳躡孼寧嚀擰獰檸聹濘紐鈕農辳儂噥濃膿衖駑釹瘧煖煗儺諾鍩謳歐毆甌鷗嘔慪漚槃盤蹣龐鉋鑤麅砲礮皰肧賠錇珮轡噴鵬掽踫紕鈹毘羆駢諞騗騙縹飃飄貧嬪頻顰評凴憑蘋缾鮃釙潑頗鉕廹僕撲鋪舖鏷樸譜鐠悽淒棲榿慼鏚齊臍頎騏騎棊碁蠐旂蘄鰭豈啓啟綺氣訖棄薺磧憇韆扡遷僉釺牽慳鉛謙諐簽籤騫蕁鈐錢鉗亁乹潛淺膁譴繾塹槧嗆羗戧槍蹌錆鏘鏹彊強墻牆嬙薔檣艢搶羥繈繦熗墝磽蹺鍫鍬繰喬僑蕎橋譙癄顦鞽誚陗竅翹竊愜篋鍥親欽琹懃鋟寢唚搇撳氫輕傾鯖檾頃請慶窮煢瓊坵秌鞦鰌鰍虯毬賕巰區粬麯嶇詘敺驅軀趨鴝臒齲闃覰覷覻權詮輇銓踡顴綣勸卻愨慤確闋闕鵲搉帬裠羣冄讓蕘饒橈擾嬈繞熱認紉姙軔靭韌飪毧絨羢榮嶸蠑螎宂銣顬縟軟輭蕋橤蘂銳叡閏潤篛灑颯薩顋鰓賽毿傘繖糝饊顙喪騷繅鰠掃澁澀嗇銫穡殺紗鎩鯊篩曬刪姍釤羶閃陝訕騸繕饍贍鱓鱔傷殤觴坰賞緔燒紹賒虵捨厙設慴懾攝灄紳詵審讅諗嬸瀋腎滲昇陞聲勝澠繩聖賸屍師蝨詩獅溼濕釃鯴時識實蝕塒蒔鰣駛勢眎眡視試飾昰柹貰適軾鈰諡謚釋壽夀獸綬書紓樞倐儵疎攄輸贖藷術樹竪豎庻數潄帥閂雙誰稅順說説爍鑠碩絲噝鷥緦螄廝鍶佀禩飼駟竢鬆慫聳訟誦頌蒐餿颼鎪擻藪甦蘇囌穌訴肅謖泝遡痠雖綏隨嵗歲誶孫猻蓀飱損筍挱簑縮嗩瑣鎖牠鉈墖獺鰨撻闥駘臺颱檯擡鮐態鈦貪攤灘癱墰壇罈壜罎曇談錟譚襢鉭嘆歎賧湯鐋鏜餳餹儻燙蹚濤絛縚縧搯韜鞀鞉討鋱騰謄籐銻綈嗁緹鵜題蹏躰體屜薙鬀闐條齠鰷覜糶銚貼鉄銕鐵厛廳聼聽烴鋌衕銅統筩慟偸媮頭禿圖凃塗釷兎團糰摶頹頽穨骽蛻飩臋託拕脫馱駝鴕鼉橢搨籜窪媧鼃襪韤膃彎灣紈翫頑輓綰盌椀萬亾網徃輞朢為爲韋圍幃溈潙違闈潿維濰偉偽僞緯葦煒瑋諉韙鮪衛衞謂餧餵蝟溫紋聞螡蟁閿脗穩問甕罋撾渦萵窩蝸臥齷烏汙汚鄔嗚誣鎢無吳蕪塢隖娬嫵廡啎憮鵡務誤騖霧鶩誒犧晳谿錫譆厀習蓆襲覡璽銑戯戲繫係餼細郤鬩潟蝦俠峽狹硤轄鎋嚇廈僊縴纖秈薟躚鍁鮮閒閑絃賢鹹嫺嫻啣銜癇鷳鷴鷼顯險獫蜆蘚縣峴莧現綫線憲餡羨獻鄉鄕薌廂緗驤鑲詳亯響餉饗鯗嚮曏項梟嘵驍綃蕭銷瀟簫嚻囂曉篠効傚嘯歗蠍協衺脅脇挾諧擕攜擷纈鞵寫洩瀉紲絏緤褻謝蠏訢鋅釁興陘倖兇洶胷脩鵂饈綉繡銹鏽須鬚頊虛噓許詡敍敘卹賉勗緒續壻漵軒諼諠萲蕿藼蘐懸鏇璿選癬絢鉉楥鞾學澩鱈謔勛勳塤壎燻尋廵馴詢潯鱘訓訊狥遜枒壓鴉鵶椏鴨啞瘂亞訝埡婭氬嚥懨懕煙臙閹醃訁閆嚴喦巖巗鹽閻顏顔簷兗儼厴縯魘鼴厭彥硯艷豔騐驗諺燄鴈灧灩釅讞饜讌醼鷰贋贗鴦揚敭颺陽楊煬瘍養癢樣殀堯餚軺窯窰謠搖遙瑤鰩葯藥鷂燿爺鋣埜壄業葉頁鄴亱曄燁爗謁靨毉醫吚銥儀詒迆飴貽迻遺頤彜彞釔艤螘蟻義億憶藝議異囈讛譯嶧懌繹詣驛軼誼縊瘞鎰瞖鐿囙陰隂蔭廕慇銦瘖陻唫婬滛銀齦飲隱癮應鶯鸎嬰嚶攖纓甖罌櫻瓔鸚鷹塋滎熒瑩螢營縈瀅鎣瀠蠅贏潁穎癭暎喲傭擁癰雝鄘鏞鱅詠湧惥慂踴優憂猶郵蓧蕕鈾遊魷銪祐誘紆餘歟魚娛諛漁崳踰覦輿與傴嶼俁語齬馭訏籲嫗飫鬱獄鈺預慾諭閾禦鵒瘉癒蕷譽鷸鳶鴛淵員園圓緣黿猨蝯轅櫞遠願約嶽鑰鈅籥悅鉞閱閲躍粵雲勻紜蕓鄖氳隕殞運鄆惲暈醖醞慍韞韻蘊帀襍雜災烖菑載簮偺喒欑儹攢趲暫賛贊讚鏨瓚賍贓贜駔髒臟塟蹧鑿棗竈皁唕譟則擇澤責嘖幘簀賾賊譖繒鋥贈摣齇紥紮剳劄軋牐閘鍘詐柵搾齋債霑氈氊譫斬盞嶄輾佔戰棧綻驏張麞漲帳脹賬釗詔趙櫂炤喆輒蟄謫讁轍鍺這淛鷓貞針鍼偵湞珎楨碪禎診軫縝陣鴆賑鎮爭徴崢掙猙鉦睜錚箏証證諍鄭幀癥巵織梔執妷姪職縶蹠躑衹隻阯紙軹誌製祑袠幟質櫛摯緻贄輊擲鷙滯騭稺穉寘觶躓終鈡鍾鐘腫種塚眾衆謅週軸箒紂呪縐晝葤皺驟硃誅諸豬銖櫧瀦櫫燭屬煑囑矚佇竚苧註貯駐築鑄筯專塼甎磚顓轉囀賺譔饌妝粧莊樁裝壯狀騅錐墜綴縋贅諄準槕斮斲斵濁諑鋜鐲茲玆貲資緇諮輜錙齜鯔姉漬眥綜椶蹤騣鬉縂總傯縱糉鄒騶諏鯫鏃詛組躦纘篹鉆鑽辠罇鱒]/;
function testTrad(text){
	return tradReg.test(text);
}
var xhr=new XMLHttpRequest();
xhr.onreadystatechange=function(){
	if(this.readyState===4){
		if(this.status===200){
			try{
				var result=eval(this.responseText)[0];
				var data=[];
				for(var i=0,len=result.length;i<len;i++){
					data.push(result[i][0]);
				}
				copyText(data.join(''));
				//showNotification('转换成功,可以粘贴了',2000);
			}catch(e){
				showNotification('转换失败>﹏﹏<',2000);
			}
		}else{
			showNotification('转换失败>﹏﹏<',2000);
		}
	}
};
function tranlateTrad(text){
	xhr.open('POST','http://translate.google.cn/translate_a/t',true);
	xhr.setRequestHeader('Content-Type','application/x-www-form-urlencoded;charset=UTF-8');
	xhr.send('client=t&text='+encodeURIComponent(text)+'&hl=en&sl=zh-TW&tl=zh-CN&ie=UTF-8&oe=UTF-8&multires=1&otf=1&pc=1&ssel=0&tsel=4');
}

function showNotification(msg,delay){
	var notify=webkitNotifications.createNotification('','',msg),handler;
	if(delay){
		notify.onshow=function(){
			handler=setTimeout(function(){
				notify.cancel();
			},delay);
		};
		notify.onclose=function(){
			clearTimeout(handler);
		};
	}
	notify.show();
}

function getByteLength(str){
	return str.replace(/[^\u0000-\u00ff]/g,'xx').length;
}

chrome.extension.onRequest.addListener(function(request,sender){
	//console.info(request,s);
	if(request.cmd == "copy" && request.data){
		var text=request.data;
		xhr.abort(); 
		copyText(text);
		if(testTrad(text)){
			//showNotification('复制内容包括繁体字,转换中...',2000);
			tranlateTrad(text);
		}
	}else if(request.cmd == "getImage"){
		getImage(request.data,sender.tab.id);
	}
});
function getImage(url,id){
	var img = new Image(),
		filename = url.replace(/^.*\//,'').split('.')[0];
	img.onload = function(){
		if(img.width<=500){
			saveImage(url,filename,id);
		}else{
			saveImage(resizeImage(img,500,Math.round(img.height*500/img.width)),filename,id);
		}
	};
	img.src = url;
}
function resizeImage($img,width,height){
	stage.width = width;
	stage.height = height;
	var context = stage.getContext('2d');
	context.drawImage($img,0,0,width,height);
	return stage.toDataURL('image/jpeg');
}
function saveImage(url,name,id){
	chrome.tabs.executeScript(id,{
		code: "var node = document.createElement('a');\
			node.href='"+url+"';\
			node.download='"+name+".jpg';\
			node.click();"
	});
}
chrome.contextMenus.create({
	type: "normal",
	title: "图片另存为(&V)...",
	contexts: ["image"],
	onclick: function(option,tab){
		getImage(option.srcUrl,tab.id);
	}
}, function(){
});
chrome.contextMenus.create({
	type: "normal",
	title: "获取本页最大图片...",
	contexts: ["page","selection","link","editable","video","audio"],
	onclick: function(option,tab){
		chrome.tabs.executeScript(tab.id,{
			file: './getlargestimage.js'
		});
	}
}, function(){
});
