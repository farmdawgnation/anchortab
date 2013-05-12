#!/usr/bin/node

var http = require('http'),
    spawn = require('child_process').spawn;

function runServerScript(scriptName, scriptCommand, onSuccess) {
  var scriptHandle = spawn(scriptCommand);

  scriptHandle.stdout.on('data', function(data) {
    process.stdout.write(data);
  });

  scriptHandle.stderr.on('data', function(data) {
    process.stderr.write(data);
  });

  scriptHandle.on('close', function(code) {
    if (code !== 0) {
      console.log(scriptName + " was unsuccessful. Aborting.");
    } else {
      console.log(scriptName + " complete.");

      if (typeof onSuccess != 'undefined') onSuccess();
    }
  });
}

function restartProduction() {
  runServerScript("Stop Production", "./production-stop.sh", function() {
    setTimeout(function() {
      runServerScript("Start Production", "./production-start.sh");
    });
  });
}

function deployProduction() {
  runServerScript("Deploy Production", "./production-deploy.sh", function() {
    restartProduction();
  });
}

http.createServer(function(req, res) {
  res.writeHead(200, {'ContentType': 'text/plain'});
  res.end('OK');

  deployProduction();
}).listen(9090, '127.0.0.1');

console.log("The CI Deploy Trigger Daemon is up on 9090.");
