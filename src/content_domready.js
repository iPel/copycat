document.addEventListener('DOMContentLoaded', function(){
	chrome.extension.sendRequest({
		cmd: 'ready'
	});
}, false);