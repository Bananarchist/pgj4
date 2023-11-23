import { WebSocket } from "ws";

function waitFor(ms) {
  return new Promise((resolve, reject) => {
    setTimeout(resolve, ms);
  });
}

function sendMove(ws, source, target) {
  ws.send(
    JSON.stringify({
      msg: "Play",
      action: {
        type: "swap",
        source, target
      }
    }));
}

function sendAttack(ws, target) {
  ws.send(
    JSON.stringify({
      msg: "Play",
      action: {
        type: "shoot",
        target
      }
    }));
}

function sendPieces(ws, five) {
  ws.send(
    JSON.stringify({
      msg: "SetPieces",
      pieces: [
        { color: "orange", honest: true },
        { color: "blue", honest: true },
        { color: "orange", honest: false },
        { color: "blue", honest: false },
        { color: "blue", honest: five },
      ],
    })
  );
}


function sendChat(ws, content) {
  ws.send(
    JSON.stringify({
      msg: "SendChat",
      content,
    })
  );
}

function sendSetName(ws, name) {
  ws.send(
    JSON.stringify({
      msg: "SetName",
      name,
    })
  );
}

function sendCreateGame(ws, name) {
  ws.send(
    JSON.stringify({
      msg: "HostGame",
      name,
    })
  );
}

function sendJoinGame(ws, gid) {
  var msg = JSON.stringify({
      msg: "JoinGame",
      gameId: gid,
    });
  console.log(msg);
  ws.send(msg);
}

function testWebSocket({ debug }) {
  var isUserList = (msg) => msg?.info == "user-list";
  var isGameList = (msg) => msg?.info == "game-list";
  var isUserJoined = (msg) => msg?.event == "user-joined";
  var isNameSet = (msg) => msg?.event == "name-set";
  var isGameAdded = (msg) => msg?.event == "game-added";
  var isPiecesRequested = (msg) => msg?.event == "pieces-requested";
  var isGameBegun = (msg) => msg?.event == "game-begun";
  var isChat = (msg) => msg?.event == "chat";
  var isGameStateUpdate = (msg) => msg?.event == "game-state-update";
  var server_ip = process.env.SERVER_IP || "localhost";

  var ws = new WebSocket(`ws://${server_ip}:9160`);
  var ws2 = new WebSocket(`ws://${server_ip}:9160`);
  var state = {
    userList: new Map(),
    gameList: new Map(),
    user_pieces: [],
    oppo_pieces: [],
  };

  var msgHandler = function (e) {
    var data = JSON.parse(e.data);
    debug(data);
    if (isUserList(data)) {
      state.userList = data.data.reduce((acc, { username, userId }) => {
        acc.set(userId, username);
        return acc;
      }, state.userList);
    } else if (isGameList(data)) {
      state.gameList = data.data.reduce((acc, { name, uuid }) => {
        acc.set(uuid, name);
        return acc;
      }, state.gameList);
    } else if (isUserJoined(data)) {
      //confirmed I guess
    } else if (isNameSet(data)) {
      //set name
    } else if (isGameAdded(data)) {
      //confirmed i guess
    } else if (isPiecesRequested(data)) {
      // "in game mode" and can send pieces
    } else if (isGameBegun(data)) {
      // maybe can start sending moves?
    } else if (isChat(data)) {
      debug(`${state.userList.get(data.sender_id)}: ${data.message}`);
    }

    //console.log(state);
  };
  ws.onmessage = msgHandler;
  ws2.onmessage = msgHandler;
  ws.onerror = function (e) {
    console.error(e);
  };
  ws2.onerror = function(e) {
    console.error(e);
  };
  ws.onopen = async function () {
    console.log("opened 1");
    ws2.onopen = async function () {
      console.log("opened 2");
      sendSetName(ws, "George");
      sendSetName(ws2, "Frank");
      sendCreateGame(ws, "George's Game");
      
      /* sendChat(ws, "Hi i'm George");
      await waitFor(250);
      sendChat(ws2, "oh hi im frank");
      await waitFor(500);
      sendChat(ws, "okay");
      await waitFor(5000);
      sendChat(ws, "bye i guess");
      */

      while (state.gameList.size == 0) {
        await waitFor(250);
      }
      sendJoinGame(ws2, state.gameList.keys().next().value);
      await waitFor(250);
      sendPieces(ws, true);
      sendPieces(ws2, false);

      await waitFor(350);
      sendMove(ws, 1, 5);
      console.log("george moved");
      await waitFor(350);
      sendAttack(ws, 1);
      console.log("george attacked");
      await waitFor(250);

      sendAttack(ws2, 2);
      await waitFor(250);
      sendAttack(ws2, 3);
      await waitFor(250);

      sendAttack(ws, 1);
      await waitFor(250);
      sendAttack(ws, 1);

    };
  };
}

testWebSocket({
  debug(evt) {
    console.log(evt);
  },
});
