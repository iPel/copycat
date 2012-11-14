window.addEventListener('keydown',function(e){
	if(e.ctrlKey && e.keyCode===67 && !e.shiftKey && !e.altKey){
		e.preventDefault();
		var data=window.getSelection().toString();
		chrome.extension.sendRequest({
			cmd: 'copy',
			data: data
		});
	}
},true);
