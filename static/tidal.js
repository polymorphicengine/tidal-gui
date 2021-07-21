function controlLoadFile(){

   var fileToLoad = document.getElementById("fileInput").files[0];
   let reader = new FileReader();

   reader.readAsText(fileToLoad);

   reader.onload = function() {
   controlEditor.getDoc().setValue(reader.result);
   };

}

function controlSaveFile()
{
    var textToSave = controlEditor.getValue();
    var textToSaveAsBlob = new Blob([textToSave], {type:"text/plain"});
    var textToSaveAsURL = window.URL.createObjectURL(textToSaveAsBlob);

    var downloadLink = document.createElement("a");
    downloadLink.download = 'untitled.tidal';
    downloadLink.innerHTML = "Download File";
    downloadLink.href = textToSaveAsURL;
    downloadLink.onclick = destroyClickedElement;
    downloadLink.style.display = "none";
    document.body.appendChild(downloadLink);

    downloadLink.click();
}

function destroyClickedElement(event)
{
    document.body.removeChild(event.target);
}
