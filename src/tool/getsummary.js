(function(){
	var getStyle = function(oElm, strCssRule){
		if(oElm == null){
			return 0;
		}
		var strValue = '0';
		if(document.defaultView && document.defaultView.getComputedStyle){
			strValue = document.defaultView.getComputedStyle(oElm, "").getPropertyValue(strCssRule);
		}
		else if(oElm.currentStyle){
			strCssRule = strCssRule.replace(/\-(\w)/g, function (strMatch, p1){
				return p1.toUpperCase();
			});
			strValue = oElm.currentStyle[strCssRule];
		}
		var nValue = parseInt(strValue, 10);
		if(isNaN(nValue)){
			return 0;
		}
		return nValue;
	}
	var getImageSize = function(img){
		var width = Math.max(img.width || img.naturalWidth, getStyle(img, 'width')) || Math.max(getStyle(img.parentNode, 'width'), 20);
		var height = Math.max(img.height || img.naturalHeight, getStyle(img, 'height')) || Math.max(getStyle(img.parentNode, 'height'), 20);
		return width * height;
	}
	var imgs=document.images;
	if(!imgs.length){
		alert('木有图片啊!亲!');
		return;
	}
	var currentImg,currentSize;
	var largestImg=imgs[0],
		size=getImageSize(largestImg);
	for(var i=1,len=imgs.length;i<len;i++){
		currentImg=imgs[i];
		currentSize=getImageSize(currentImg);
		if(currentSize > size){
			var width = currentImg.width || getStyle(currentImg, 'width');
			var height = currentImg.height || getStyle(currentImg, 'height');
			if(width && height){
				var rate = width / height;
				if(rate > 3 || rate < .33){
					continue;
				}
			}
			largestImg = currentImg;
			size = currentSize;
		}
	}
	var summary = '',
		target = largestImg.parentNode;
	for(var i=0;i<10 && target;i++){
		summary = target.innerText;
		if(summary.length > 50){
			break;
		}
		target = target.parentNode;
	}
	summary = summary.replace(/\n\d+\/\d+\n/g,'\n\n').replace(/\n\d+\/\d+\n/g,'\n\n').trim().replace(/\n+/g,'\n');
	chrome.extension.sendMessage({
		cmd: "getSummary",
		data: {
			url: largestImg.src,
			text: summary,
			page: location.href
		}
	});
})();