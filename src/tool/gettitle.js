(function(){
list = document.querySelectorAll('*');
re=[];
for(var i=0,len=list.length;i<len;i++){
	var el = list[i];
	if((/H\d/).test(el.tagName) || (/title/i).test(el.id) || (/title/i).test(el.className)){
		if(el.querySelector('div,p')){
			continue;
		}
		var text = el.innerText.trim(),
			size = parseInt(el.style.fontSize || document.defaultView.getComputedStyle(el).getPropertyValue('font-size')),
			length = text.length;
		if((/[\n\r]/).test(text)){
			continue;
		}
		re.push({
			text:text,
			size:size,
			len:length,
			rank:size*length
		});
	}
}
re.sort(function(a,b){
	return b.rank - a.rank || b.len-a.len;
});
return re[0].text;
})();