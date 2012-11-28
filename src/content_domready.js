document.addEventListener('DOMContentLoaded', function(){
	chrome.extension.sendMessage({
		cmd: 'ready'
	});
}, false);