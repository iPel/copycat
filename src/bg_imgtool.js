(function(ns, undefined){
	"use strict";
	var context = ns['image'] = {};
	var $canvas = document.createElement('canvas'),
		autoSaveState = 0, //0: stop, 1:running, 2:planning stop
		pageLeft = -1,
		pageTotal = 0,
		currentTabId = -1,
		_newWindow;
	var EMPTY_FUNC = function(){};

	var onNewWindowKeyDown = function(e){
		if(e.ctrlKey && e.keyCode===67 && !e.shiftKey && !e.altKey){
			e.preventDefault();
			var data=e.currentTarget.getSelection().toString();
			CC.handleCmd({
				cmd: 'copy',
				data: data
			});
		}
	};
	var getNewWindow = function(){
		if(_newWindow && _newWindow.window){
		}else{
			_newWindow = window.open();
			_newWindow.document.title = '图集工具输出';
			_newWindow.document.write('<style>\
					div { border:1px solid #ccc; margin:5px 10px; padding:10px; white-space:pre-line; }\
					img { max-height:100px; max-width:100px; }\
					em { color:#933; }\
					.wrap { float:left; margin-right:10px }\
					.link { padding:0; margin:0; clear:both; display:block; overflow:hidden; }\
					.link a { text-decoration:none; color:#999; font-size:10px; white-space:nowrap; }\
					.link a:hover { color:#99c; }\
				</style>');
			_newWindow.addEventListener('keydown',onNewWindowKeyDown,true);
		}
		return _newWindow;
	};
	var getImage = function(url,id){
		var img = new Image(),
			filename = url.replace(/^.*\//,'').split('.')[0],
			m = url.match(/\.(jpg|jpeg|png|gif)\b/ig),
			ext = m?m[0]:'.jpg';
		img.onload = function(){
			var option;
			if(img.width<=500 && img.height<=500){
				option = {
					imgData: url,
					name: filename,
					ext: ext,
					id: id
				};
			}else if(img.width>img.height){
				option = {
					imgData: resizeImage(img,500,Math.round(img.height*500/img.width)),
					name: filename,
					ext: '.jpg',
					id: id
				};
			}else{
				option = {
					imgData: resizeImage(img,Math.round(img.width*500/img.height),500),
					name: filename,
					ext: '.jpg',
					id: id
				};
			}
			saveImage(option);
		};
		img.src = url;
	};
	var resizeImage = function($img,width,height){
		$canvas.width = width;
		$canvas.height = height;
		var context = $canvas.getContext('2d');
		context.globalCompositeOperation="source-over";
		context.drawImage($img,0,0,width,height);
		context.globalCompositeOperation="destination-over";
		context.fillStyle="#fff";
		context.fillRect(0,0,width,height);
		return $canvas.toDataURL('image/jpeg',.8);
	};
	var saveImage = function(option){
		chrome.tabs.executeScript(option.id,{
			code: "var node = document.createElement('a');\
				node.href='"+option.imgData+"';\
				node.download='"+option.name+option.ext+"';\
				var click=document.createEvent('MouseEvent');\
				click.initEvent('click');\
				node.dispatchEvent(click);"
		});
	};
	var getSummary = function(url,text,page){
		var img = new Image(),
			filename = url.replace(/^.*\//,'').split('.')[0],
			m = url.match(/\.(jpg|jpeg|png|gif)\b/ig),
			ext = m?m[0]:'.jpg';
		img.onload = function(){
			var option;
			if(img.width * img.height < 40000){
				text = '<em>图片太小请检查</em>' + text;
			}
			if(img.width<=500 && img.height<=500){
				option = {
					imgData: url,
					name: filename,
					ext: ext,
					text: text,
					origin: url,
					page: page
				};
			}else if(img.width>img.height){
				option = {
					imgData: resizeImage(img,500,Math.round(img.height*500/img.width)),
					name: filename,
					ext: '.jpg',
					text: text,
					origin: url,
					page: page
				};
			}else{
				option = {
					imgData: resizeImage(img,Math.round(img.width*500/img.height),500),
					name: filename,
					ext: '.jpg',
					text: text,
					origin: url,
					page: page
				};
			}
			outputSummary(option);
		};
		img.src = url;
	}
	var outputSummary = function(option){
		getNewWindow().document.write('<div>\
			<a class="wrap" href="'+option.imgData+'" download="'+option.name+option.ext+'" title="点击保存"><img src="'+option.imgData+'" /></a>' +
			option.text + '<p class="link"><a target="_blank" href="'+option.page+'">'+option.page+'</a></p></div>');
		pageLeft--;
		if(pageLeft && autoSaveState === 1){
			chrome.tabs.executeScript(currentTabId,{
				code: "try{var imgs = document.querySelectorAll('img[src]'),target;\
					for(var i=0,len=imgs.length;i<len;i++){\
						if(imgs[i].src=='"+option.origin+"'){\
							target = imgs[i];\
							break;\
						}\
					}\
					target.scrollIntoView();\
					var x = target.offsetLeft + target.offsetWidth * .8,\
						y = target.offsetTop + target.offsetHeight * .5;\
					while(target = target.offsetParent){\
						x += target.offsetLeft;\
						y += target.offsetTop;\
					}\
					var node = document.elementFromPoint(x - pageXOffset, y - pageYOffset),\
						click=document.createEvent('MouseEvent');\
					click.initMouseEvent('click', true, true, window, 1, 0, 0,\
						Math.round(node.offsetWidth * .8), Math.round(node.offsetHeight * .5), false, false, false, false, 0, null);\
					node.dispatchEvent(click);}catch(e){alert('出错了!请手动点下一页');}"
			});
		}else{
			context.stopImgTool();
			autoSaveState = 0;
			CC.showNotification('图集工具已停止', 2000);
		}
	};
	var stepImgTool = function(){
		chrome.tabs.executeScript(currentTabId,{
			file: './tool/getsummary.js'
		});
	};
	context.startImgTool = function(pageCount, tabId){
		pageLeft = pageCount;
		pageTotal = pageCount;
		currentTabId = tabId;
		autoSaveState = 1;
		chrome.tabs.executeScript(currentTabId,{
			code: "window.addEventListener('hashchange',function(e){\
					chrome.extension.sendMessage({cmd: 'ready'});\
				},false);"
		});
		stepImgTool();
	};
	context.stopImgTool = function(){
		if(!autoSaveState){
			return;
		}
		autoSaveState = 2;
		currentTabId = -1;
		pageLeft = -1;
		pageTotal = 0;
	};
	context.getImgToolState = function(){
		return {
			'state' : autoSaveState,
			'progress' : 1 - pageLeft / pageTotal
		};
	}

	CC.addCmd('getImage', function(data,sender){
		getImage(data,sender.tab.id);
	});
	CC.addCmd('ready', function(data,sender){
		if(autoSaveState && sender.tab.id === currentTabId){
			setTimeout(stepImgTool, 1000);
		}
	});
	CC.addCmd('getSummary', function(data,sender){
		getSummary(data.url, data.text, data.page);
	});
	chrome.contextMenus.create({
		type: "normal",
		title: "图片另存为(&V)...",
		contexts: ["all"],
		onclick: function(option,tab){
			if(option.mediaType === 'image'){
				getImage(option.srcUrl,tab.id);
			}else{
				setTimeout(function(){
				chrome.tabs.sendMessage(tab.id, {
					cmd: 'getImageByPoint'
				});
			},0);
			}
		}
	}, EMPTY_FUNC);
	// chrome.contextMenus.create({
	// 	type: "normal",
	// 	title: "获取本页最大图片...",
	// 	contexts: ["page","selection","link","editable","video","audio"],
	// 	onclick: function(option,tab){
	// 		chrome.tabs.executeScript(tab.id,{
	// 			file: './tool/getlargestimage.js'
	// 		});
	// 	}
	// }, EMPTY_FUNC);
})(CC);
