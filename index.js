'use strict'
const morph = require('nanomorph')
const syncData = require('dact-electron')
const {ipcRenderer} = require('electron')
const main = require('./elements/main')
const {readLog, writeLog} = require('./utils/log')
const createData = require('./modules/createData')
const {mergeLog} = require('./modules/log')
const {setTitle} = require('./modules/timer')

const log = readLog()
const data = createData(syncData(ipcRenderer))
const latestTitle = log.length ? log[log.length - 1].title : ''

data.emit(mergeLog, log)
data.emit(setTitle, latestTitle)

const body = document.querySelector('body')
let tree = body.appendChild(main(data.state, data.emit))

data.subscribe(() => {
  tree = morph(tree, main(data.state, data.emit))
})

data.subscribe('log', () => {
  writeLog(data.state.log)
})
