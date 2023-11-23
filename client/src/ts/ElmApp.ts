export type ElmApp = {
  ports: {
    setLocalStorage: Subscriber<Data>
    clearLocalStorage: Subscriber<Data>
    
  }
}

type Data =
  | null
  | { "somekey": string }

type Subscriber<T> = {
  subscribe(callback: (param: T) => void) : void
}

type Sender<T> = {
  send: T
}
