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

(function() {

  function getFile(id) {
    return Document.getElementById(id);
  }

  function onFileChange(listener, file) {
    var reader = new FileReader();
    reader.onload = function(e) {
      listener._contents =
        { name: file.name,
          lastModified: file.lastModified,
          type: file.type,
          size: file.size,
          data: e.target.result
        };
      listener.dispatchEvent(new CustomEvent('load'));
    }
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
      var file = file._file;
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
          file._onChange = function() {
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
