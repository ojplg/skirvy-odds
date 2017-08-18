function populateForm(){
  populateDropDown("attacker", 2);
  populateDropDown("defender", 1);
}

function populateDropDown(selectId, start){
  var select = document.getElementById(selectId);
  for(var i = start; i<25 ; i++){
    var opt = document.createElement("option");
    opt.textContent = i;
    opt.value = i;
    select.appendChild(opt);
  }
}
