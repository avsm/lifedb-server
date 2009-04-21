/*
 * Ext JS Library 3.0 RC1
 * Copyright(c) 2006-2009, Ext JS, LLC.
 * licensing@extjs.com
 * 
 * http://extjs.com/license
 */

Ext.BLANK_IMAGE_URL = 'resources/images/default/s.gif';

Ext.onReady(function(){

    var User = Ext.data.Record.create([
      {name: 'Username', mapping: 'uid'},
      {name: 'Hostname', mapping: 'ip'},
      {name: 'Port', mapping: 'port'},
      {name: 'Key', mapping: 'key'},
    ]);

    // create the Data Store
    var store = new Ext.data.GroupingStore({

        proxy: new Ext.data.HttpProxy({            
            method: 'GET',
            url:'/user'
        }),

        // the return will be XML, so lets set up a reader
        reader: new Ext.data.JsonReader({
            totalProperty: 'results',
            root: 'rows',
        }, User)
    });

    var fm = Ext.form;

    var editor = new Ext.ux.RowEditor({
        saveText: 'Update'
    });

    // create the grid
    var grid = new Ext.grid.GridPanel({
        store: store,
        plugins: [editor],
        view: new Ext.grid.GroupingView({
            markDirty: false
        }),

        columns: [
            {header: "Username", dataIndex: 'Username', sortable: true,
              editor: new fm.TextField({
                 allowBlank: false,
              })
            },
            {header: "Hostname", dataIndex: 'Hostname', sortable: true,
              editor: new fm.TextField({
                 allowBlank: false
              })
            },
            {header: "Port", dataIndex: 'Port', sortable: true,
              editor: {
                 allowBlank: false,
                 xtype: 'numberfield',
                 minValue: 1,
                 maxValue: 65535,
              }
            },
            {header: "Key", dataIndex: 'Key', sortable: false},
        ],
        renderTo:'user-grid',
        clicksToEdit:1,
        width:540,
        height:200,
        tbar: [{
            iconCls: 'icon-user-add',
            text: 'Add User',
            handler : function(){
               var u = new User({
                 Username: '',
                 Hostname: 'example.com',
                 Port: 5985,
                 Key: "",
               });
               editor.stopEditing();
               store.insert(0, u);
               grid.getView().refresh();
               grid.getSelectionModel().selectRow(0);
               editor.startEditing(0);
            }
        },
        {
           ref : '../removeBtn',
           iconCls: 'icon-user-delete',
           disabled: true,
           text: 'Delete User',
           handler : function () {
             grid.stopEditing();
             var s = grid.getSelectionModel().getSelections();
             for(var i = 0, r; r = s[i]; i++){
               Ext.Ajax.request({
                 url: '/user/' + r.get('Username'),
                 method : 'DELETE',
                 success: function(request, result) {
                   store.reload ();
                 },
                 failure: function(request, result) {
                   Ext.Msg.alert('User', 'Deletion failed!');
                 },
               });
             }
           }
        }
        ]
    });

    grid.getSelectionModel().on('selectionchange', function(sm){
        grid.removeBtn.setDisabled(sm.getCount() < 1);
    });

    function applyUserChanges() {
      console.log('applyUserChanges');
      mr = this.getStore().getModifiedRecords();
      for (var i = 0; i < mr.length; i++) {
        var r = mr[i];
        var j = {
          uid: r.get('Username'),
          ip: r.get('Hostname'),
          port: r.get('Port'),
          key: r.get('Key'),
        };
        Ext.Ajax.request( {
          waitMsg: "saving user...",
          url: '/user',
          method: 'POST',
          jsonData:  j,
          success: function (request, result) {
            r.commit();
          },
          failure: function (request, result) {
            Ext.Msg.alert('User', 'Failed to edit user: ' + result.responseText);
          },
        });
      }
      this.getStore().reload();
    }

    store.on('update', applyUserChanges, grid);
    store.load();
});