(function(){
	var imgs=document.images;
	if(!imgs.length){
		alert('木有图片啊!亲!');
		return;
	}
	var currentImg,currentSize,rate;
	var largestImg=imgs[0],
		size=largestImg.width*largestImg.height;
	for(var i=1,len=imgs.length;i<len;i++){
		currentImg=imgs[i];
		currentSize=currentImg.width*currentImg.height;
		rate=currentImg.width/currentImg.height;
		if(currentSize > size && rate<3 && rate>.33){
			largestImg = currentImg;
			size = currentSize;
		}
	}
	chrome.extension.sendRequest({
		cmd: "getImage",
		data: largestImg.src
	});
})();