var point;
var getImageByPoint = function(){
	if(!point){
		return;
	}
	var targets = [],el;
	// window.scrollTo(point.x,point.y);
	for(;;){
		el = document.elementFromPoint(point.x - window.pageXOffset, point.y - window.pageYOffset);
		if(!el){
			 break;
		}else if(el.tagName === 'IMG'){
			setTimeout(function(){
				chrome.extension.sendMessage({
					cmd: "getImage",
					data: el.src
				});
			},0);
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
	point = null;
};
var onKeyDown = function(e){
	if(e.ctrlKey && e.keyCode===67 && !e.shiftKey && !e.altKey){
		e.preventDefault();
		var data=window.getSelection().toString();
		chrome.extension.sendMessage({
			cmd: 'copy',
			data: data
		});
	}
};
chrome.storage.local.get('enableTextTool', function(items){
	if(items['enableTextTool']) {
		window.addEventListener('keydown', onKeyDown, true);
	}
	chrome.storage.onChanged.addListener(function(changes, area){
		var field;
		if (area == 'local' && (field = changes['enableTextTool'])) {
			window[field.newValue?'addEventListener':'removeEventListener']('keydown', onKeyDown, true);
		}
	});
});
window.addEventListener('contextmenu',function(e){
	point = {
		x:e.pageX,
		y:e.pageY
	};
	chrome.extension.sendMessage({
		cmd: 'setPicFlag',
		data: e.ctrlKey
	});
},true);
window.addEventListener('click',function(){point = null;},false);
chrome.extension.onMessage.addListener(function(request, sender){
	if(request.cmd === 'getImageByPoint'){
		getImageByPoint();
	}
});