(function(){
	var meta = document.querySelector('meta[name=keywords]');
	var keywords = meta && meta.getAttribute('content');
	if(keywords){
		var node = document.createElement('div');
		node.style.cssText = 'position:absolute;right:0;top:50px;z-index:99999;font-size:12px;';
		node.innerHTML = '关键词<a href="#" style="float:right" onclick="this.parentNode.parentNode.removeChild(this.parentNode);">X 关闭</a>\
			<textarea style="display:block;width:150px;height:100px;">'
			+keywords.replace(/&/g,"&amp;").replace(/>/g,"&gt;").replace(/</g,"&lt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")
    		+'</textarea>';
    	document.body.appendChild(node);
	}
})();
