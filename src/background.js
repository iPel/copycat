(function(ns, undefined){
	"use strict";
	var context = ns['CC'] = {};
	var remoteCommands = {};
	context.addCmd = function(cmd, handler){
		if(remoteCommands[cmd]){
			remoteCommands[cmd].push(handler);
		}else{
			remoteCommands[cmd] = [handler];
		}
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
			for(var cmds = remoteCommands[request.cmd], i = 0, len = cmds.length; i < len; i++){
				try{
					cmds[i](request.data,sender);
				}catch(e){
					console.error('error on ' + request.cmd, e);
				}
			}
		}
	};
	chrome.extension.onMessage.addListener(context.handleCmd);
})(window);
