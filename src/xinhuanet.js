window.addEventListener('mouseup',function(e){
	if(e.target.nodeName=='IMG'){
		e.stopPropagation();
	}
},true);