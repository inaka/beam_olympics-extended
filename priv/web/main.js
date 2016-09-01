//
// INITIALIZATION
//
let table = document.getElementById("scoreTable");
let socket = null;

//
// TABLE MANAGEMENT UTILS
//
function addUser(name, completed, toComplete, score)
{
  let row = table.insertRow(1);
  let nameCell = row.insertCell(0);
  let todoCell = row.insertCell(1);
  let scoreCell = row.insertCell(2);

  nameCell.innerHTML = name;
  todoCell.innerHTML = completed + "/" + toComplete;
  scoreCell.innerHTML = score;

  sortByScore();
}

function updateUser(name, completed, toComplete, score)
{
  // Find user in the table
  let l = table.rows.length;
  for (let i = 0; i < l; i++)
  {
    let row = table.rows[i];
    if (row.cells[0].innerHTML == name)
    {
      row.cells[1].innerHTML = completed + "/" + toComplete;
      row.cells[2].innerHTML = score;
    }
  }
  sortByScore();
}

function sortByScore()
{
  let a = [];
  let l = table.rows.length - 1;
  for (let i = 0; i < l; i++)
  {
    let row = table.rows[i + 1];
    a[i] = [row.cells[0].innerHTML, row.cells[1].innerHTML, parseInt(row.cells[2].innerHTML)];
  }
  // Sort
  a.sort((a, b) => a[2] > b[2] ? -1 : 1);
  // Just in case, remove the duplicates
  let i = 1;
  while (i < a.length)
  {
    if (a[i][0] == a[i - 1][0])
      a.splice(i, 1);
    else
      i++;
  }
  l = a.length;
  // Update the table
  for (let i = 0; i < l; i++)
  {
    let row = table.rows[i + 1];
    row.cells[0].innerHTML = a[i][0];
    row.cells[1].innerHTML = a[i][1];
    row.cells[2].innerHTML = a[i][2];
  }
  // Remove the extra rows (if any)
  while (table.rows.length > l + 1)
  {
    table.deleteRow(table.rows.length - 1);
  }
}

//
// NETWORK UTILS
//
function connect()
{
  console.log("INFO: establishing connection");
  // Create a websocket
  socket = new WebSocket("ws://" + window.location.host + "/ws");
  // Set all the socket callbacks
  socket.onopen = onOpen;
  socket.onclose = onClose;
  socket.onmessage = onMessage;
}

function onClose(e)
{
  console.log("ERROR: connection lost, reconnecting");

  setTimeout(connect, 3000);
}

function onOpen(e)
{
  console.log("INFO: connection established");
}

function onMessage(e)
{
  var msg = JSON.parse(event.data);

  switch(msg.event)
  {
    case "changed":
      updateUser(msg.name, msg.currentTasks, msg.totalTasks, msg.score);
      break;
    case "added":
      addUser(msg.name, msg.currentTasks, msg.totalTasks, msg.score);
      break;
    default:
      console.log("WARN: unexpected event " + event.data);
  }
}

//
// MAIN LOOP
//
connect();