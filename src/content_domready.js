document.addEventListener('DOMContentLoaded',function(e){
	chrome.extension.sendRequest({
		cmd: 'ready'
	});
},false);