(function(ns, undefined){
	"use strict";
	var context = ns['CC'] = {};
	var remoteCommands = {};
	context.addCmd = function(cmd, handler){
		remoteCommands[cmd] = handler;
	};
	context.showNotification = function(msg,delay){
		var notify=webkitNotifications.createNotification('','',msg),handler;
		if(delay){
			notify.onshow=function(){
				handler=setTimeout(function(){
					notify.cancel();
				},delay);
			};
			notify.onclose=function(){
				clearTimeout(handler);
			};
		}
		notify.show();
	};
	context.handleCmd = function(request,sender){
		//console.info(request);
		if(request.cmd in remoteCommands){
			remoteCommands[request.cmd](request.data,sender);
		}
	};
	chrome.extension.onMessage.addListener(context.handleCmd);
})(window);
