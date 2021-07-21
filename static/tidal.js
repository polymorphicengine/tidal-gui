function controlLoadFile(){

   var fileToLoad = document.getElementById("fileInput").files[0];
   let reader = new FileReader();

   reader.readAsText(fileToLoad);

   reader.onload = function() {
   controlEditor.getDoc().setValue(reader.result);
   };

}
