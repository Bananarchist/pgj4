function openSocket(app, ip, port) {
	socket = new WebSocket(`ws://${ip}:${port}`);
	socket.addEventListener("open", (event) => {
		app.ports.socketOpened.send("");
	});
	socket.addEventListener("message", (event) => {
		app.ports.socketMessageReceiver.send(event.data);
		console.log(event)
	});
	socket.addEventListener("close", (event) => {
		app.ports.socketClosed.send("");
	});
	socket.addEventListener("error", (event) => {
		console.log(event);
		app.ports.socketClosed.send("Couldn't open socket");
	});
	app.ports.send.subscribe((event) => {
		console.log(event);
		socket.send(event);
	});
}

