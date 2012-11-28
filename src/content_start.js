var point;
var getImageByPoint = function(){
	if(!point){
		alert('你都没有点到图片!');
	}
	var targets = [],el;
	window.scrollTo(point.x,point.y);
	for(;;){
		el = document.elementFromPoint(point.x - window.pageXOffset, point.y - window.pageYOffset);
		if(el.tagName === 'IMG'){
			chrome.extension.sendMessage({
				cmd: "getImage",
				data: el.src
			});
			break;
		}else if(el === document.body){
			alert('你都没有点到图片!');
			break;
		}else{
			targets.push({
				el:el,
				value:el.style.visibility
			});
			el.style.visibility = 'hidden';
		}
	}
	for(var target;target = targets.pop();){
		target.el.style.visibility = target.value;
	}
};
window.addEventListener('keydown',function(e){
	if(e.ctrlKey && e.keyCode===67 && !e.shiftKey && !e.altKey){
		e.preventDefault();
		var data=window.getSelection().toString();
		chrome.extension.sendMessage({
			cmd: 'copy',
			data: data
		});
	}
},true);
window.addEventListener('mousedown',function(e){
	if(e.button === 2){
		point = {
			x:e.pageX,
			y:e.pageY
		}
	}else{
		point = null;
	}
},true);
chrome.extension.onMessage.addListener(function(request, sender){
	if(request.cmd === 'getImageByPoint'){
		getImageByPoint();
	}
});