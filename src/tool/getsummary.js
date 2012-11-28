(function(){
	var imgs=document.images;
	if(!imgs.length){
		alert('木有图片啊!亲!');
		return;
	}
	var currentImg,currentSize,rate;
	var largestImg=imgs[0],
		size=largestImg.naturalWidth*largestImg.naturalHeight;
	for(var i=1,len=imgs.length;i<len;i++){
		currentImg=imgs[i];
		currentSize=currentImg.naturalWidth*currentImg.naturalHeight;
		rate=currentImg.naturalWidth/currentImg.naturalHeight;
		if(currentSize > size && rate<3 && rate>.33){
			largestImg = currentImg;
			size = currentSize;
		}
	}
	var summary = '',
		target = largestImg.parentNode;
	for(var i=0;i<10 && target;i++){
		summary = target.innerText;
		if(summary.length > 10){
			break;
		}
		target = target.parentNode;
	}
	chrome.extension.sendMessage({
		cmd: "getSummary",
		data: {
			url: largestImg.src,
			text: summary.trim(),
			page: location.href
		}
	});
})();