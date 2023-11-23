import * from './ElmApp.ts';

function initializeLocalStorage(app: ElmApp) {
		app.ports.setLocalStorage.subscribe(dictstring => {
				let dict = JSON.parse(dictstring)
				for (const k in dict) {
						localStorage.setItem(k, dict[k]);
				}
		});
		app.ports.clearLocalStorage.subscribe(() => {
				localStorage.clear();
		});
		app.ports.getLocalStorage.subscribe(k => {
				app.ports.localStorage.send(localStorage.getItem(k));
		});
}
