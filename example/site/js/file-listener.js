//////////////////////////////////////////////////////////////////////
//
// file-listener.js
// Define the `file-listener` custom element.
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

/* Debugging code
var file;
var body;
var listener;

function setupFileListener() {
  file = document.createElement('input');
  file.id = 'thefile';
  file.type = 'file';
  body = document.children[0].children[1];
  body.appendChild(file);
  listener = document.createElement('file-listener');
  body.appendChild(listener);
  listener.fileId = 'thefile';
}
*/

(function() {

  function getFile(id) {
    return document.getElementById(id);
  }

  function onFileChange(listener, file) {
    var files = file.files;
    if (!files || files.length < 1) return;
    file = files[0];

    var reader = new FileReader();
    reader.onload = function(e) {
      listener._contents =
        { name: file.name,
          lastModified: file.lastModified,
          type: file.type,
          size: file.size,
          data: e.target.result
        };
      console.log('Loaded:', listener._contents);
      listener.dispatchEvent(new CustomEvent('load'));
    }
    console.log('Reading:', file);
    reader.readAsBinaryString(file);
  }

  customElements.define('file-listener', class extends HTMLElement {
    constructor() {
      super();

      // Properties
      this._fileId = null;
      this._contents = null;

      // State
      this._file = null;
      this._onChange = null;
    }

    get contents() {
      return this._contents;
    }

    get fileId() {
      return this._fileId
    }

    set fileId(value) {
      if (this._fileId === value) return;
      var file = this._file;
      if (file) {
        file.removeEventListener('change', this._onChange);
        this._file = null;
        this._onChange = null;
      }
      var fileId = value;
      this._fileId = fileId;
      if (fileId) {
        file = getFile(fileId);
        if (file) {
          this._file = file;
          var listener = this;
          listener._onChange = function() {
            // `this` should be the file here. Use `file` instead?
            onFileChange(listener, this);
          };
          // Will Elm's DOM synchronization remove this? We'll see.
          file.addEventListener('change', this._onChange, false);
        }
      }
    }

    connectedCallback() {
      // Move along. Nothing to see here.
    }
  })

})();