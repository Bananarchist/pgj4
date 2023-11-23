function initializeLocalStorage(app) {
  app.ports.saveLocalStorage.subscribe((dictstring) => {
    let dict = JSON.parse(dictstring);
    for (const k in dict) {
      localStorage.setItem(k, JSON.stringify(dict[k]));
    }
  });
  /* app.ports.clearLocalStorage.subscribe(() => {
				localStorage.clear();
		}); */
  app.ports.getLocalStorage.subscribe((keys) => {
    const data = {};
    if (keys.every((k) => localStorage.hasOwnProperty(k))) {
      app.ports.localStorage.send(
        JSON.stringify(
          keys.reduce((acc, k) => {
            console.log(acc, k);
            let v = localStorage.getItem(k);
            return Object.assign(acc, { [k]: JSON.parse(v) }); 
          }, {})
        )
      );
    }
  });
}
