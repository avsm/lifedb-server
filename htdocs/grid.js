/*
 * Ext JS Library 3.0 RC1
 * Copyright(c) 2006-2009, Ext JS, LLC.
 * licensing@extjs.com
 * 
 * http://extjs.com/license
 */

Ext.BLANK_IMAGE_URL = 'resources/images/default/s.gif';

Ext.onReady(function(){

    var fm = Ext.form;

    var Filter = Ext.data.Record.create([
      {name: 'Name', mapping: 'name'},
      {name: 'Rule', mapping: 'body'},
      {name: 'Order', mapping: 'zorder'},
    ]);

    var filter_proxy = new Ext.data.HttpProxy({
      method: 'GET',
      url : '/filter/_unknown'
    });

    var filter_store = new Ext.data.GroupingStore({
      proxy: filter_proxy,
      reader: new Ext.data.JsonReader({
        totalProperty: 'results',
        root: 'rows',
      }, Filter) 
    });

    var filter_editor = new Ext.ux.RowEditor({
      saveText: "Update"
    });

    var rules = new Ext.data.ArrayStore({
      fields: ['rule'],
      data: [ ['add *'], ['add * where #remote in recipients'] ]
    });
 
    var filter_grid = new Ext.grid.GridPanel({
      store: filter_store,
      collapsible: true,
      title: 'Filters',
      plugins: [filter_editor],
      clicksToEdit: 1,
      width: 700,
      height: 150,
      view: new Ext.grid.GroupingView({ markDirty: false }),
      columns: [
        {header: "Name", dataIndex:"Name", sortable: true,
          editor: new fm.TextField({ allowBlank: false})},
        {header: 'Rule', dataIndex:'Rule', sortable: true, width: 200,
          editor: { xtype:'combo', triggerAction:'all', forceSelection: true,
            store:['add *', 'add * where #remote in recipients'], typeAhead:true,
            mode:'local', selectOnFocus:true, emptyText: 'Select rule...'}},
        {header: "Order", dataIndex:"Order", sortable: true,
          editor: {
            allowBlank: false,
            xtype: 'numberfield',
            minValue: 1,
            maxValue: 200,
          }
        }
      ],
      tbar: [{
          iconCls: 'icon-feed-add',
          ref : '../addBtn',
          text: 'Add Rule',
          disabled: true,
          handler : function(){
            var u = new Filter({
              Name: '',
              Body: '',
              Order: 1,
            });
            filter_editor.stopEditing();
            filter_store.insert(0, u);
            filter_grid.getView().refresh();
            filter_grid.getSelectionModel().selectRow(0);
            filter_editor.startEditing(0);
          }
        },
        {
          ref : '../removeBtn',
          iconCls: 'icon-feed-delete',
          disabled: true,
          text: 'Delete Rule',
          handler : function () {
             filter_grid.stopEditing();
             var u = filter_store.current_user;
             var s = filter_grid.getSelectionModel().getSelections();
             for(var i = 0, r; r = s[i]; i++) {
               Ext.Ajax.request({
                 url: '/filter/' + u + "/" + r.get('Name'),
                 method : 'DELETE',
                 success: function(request, result) {
                   filter_store.reload ();
                 },
                 failure: function(request, result) {
                   Ext.Msg.alert('Filter', 'Deletion failed!');
                 },
               });
             }
           }
        }
        ]

    });

    function applyFilterChanges() {
      console.log('applyFilterChanges');
      mr = this.getStore().getModifiedRecords();
      for (var i = 0; i < mr.length; i++) {
        var r = mr[i];
        var j = {
          name: r.get('Name'),
          body: r.get('Rule'),
          zorder: r.get('Order'),
        };
        Ext.Ajax.request( {
          waitMsg: "saving filter...",
          url: '/filter/' + filter_store.current_user,
          method: 'POST',
          jsonData:  j,
          success: function (request, result) {
            r.commit();
          },
          failure: function (request, result) {
            Ext.Msg.alert('Filter', 'Failed to edit filter: ' + result.responseText);
          },
        });
      }
      this.getStore().reload();
    }

    filter_store.on('update', applyFilterChanges, filter_grid);

    filter_grid.getSelectionModel().on('selectionchange', function(sm){
        filter_grid.removeBtn.setDisabled(sm.getCount() < 1);
    });

    var User = Ext.data.Record.create([
      {name: 'Username', mapping: 'uid'},
      {name: 'Hostname', mapping: 'ip'},
      {name: 'Port', mapping: 'port'},
      {name: 'Key', mapping: 'key'},
    ]);

    // create the Data Store
    var user_store = new Ext.data.GroupingStore({

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

    var user_editor = new Ext.ux.RowEditor({
        saveText: 'Update'
    });

    // create the user_grid
    var user_grid = new Ext.grid.GridPanel({
        store: user_store,
        title: 'Users',
        plugins: [user_editor],
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
        clicksToEdit:1,
        width:700,
        height: 200,
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
               user_editor.stopEditing();
               user_store.insert(0, u);
               user_grid.getView().refresh();
               user_grid.getSelectionModel().selectRow(0);
               user_editor.startEditing(0);
            }
        },
        {
           ref : '../removeBtn',
           iconCls: 'icon-user-delete',
           disabled: true,
           text: 'Delete User',
           handler : function () {
             user_grid.stopEditing();
             var s = user_grid.getSelectionModel().getSelections();
             for(var i = 0, r; r = s[i]; i++){
               Ext.Ajax.request({
                 url: '/user/' + r.get('Username'),
                 method : 'DELETE',
                 success: function(request, result) {
                   user_store.reload ();
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

    user_grid.getSelectionModel().on('selectionchange', function(sm){
        user_grid.removeBtn.setDisabled(sm.getCount() < 1);
        filter_grid.addBtn.setDisabled(sm.getCount() < 1);
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

    user_store.on('update', applyUserChanges, user_grid);

    user_grid.getSelectionModel().on('rowselect', function(sm, rowIdx, r) {
      filter_store.proxy = new Ext.data.HttpProxy({
        method: 'GET',
        url : '/filter/' + r.get('Username')
      });
      filter_grid.collapse ();
      filter_store.removeAll();
      filter_grid.setTitle("Filters for " + r.get('Username'));
      filter_store.current_user = r.get('Username');
      filter_store.reload ();
      filter_grid.expand ();
      console.log(r);
    });

    user_panel = new Ext.Panel({
      title: 'User Administration',
      collapsible:true,
      items: [ user_grid, filter_grid ],
    });

    // IN TASK 

    var InTask = Ext.data.Record.create([
      {name: 'Name', mapping: 'name'},
      {name: 'Plugin', mapping: 'plugin'},
      {name: 'Mode', mapping: 'mode'},
      {name: 'Silo', mapping: 'silo'},
      {name: 'Period', mapping: 'period'},
      {name: 'Args', mapping: 'args'},
      {name: 'Pid', mapping: 'pid'},
    ]);

    // create the Data Store
    var in_task_store = new Ext.data.GroupingStore({

        proxy: new Ext.data.HttpProxy({
            method: 'GET',
            url:'/intask'
        }),

        // the return will be XML, so lets set up a reader
        reader: new Ext.data.JsonReader({
            totalProperty: 'results',
            root: 'rows',
        }, InTask)
    });

    var in_task_editor = new Ext.ux.RowEditor({
        saveText: 'Update'
    });

    // create the in_task_grid
    var in_task_grid = new Ext.grid.GridPanel({
        store: in_task_store,
        title: 'Tasks',
        plugins: [in_task_editor],
        view: new Ext.grid.GroupingView({
            markDirty: false
        }),

        columns: [
            {header: "Name", dataIndex: 'Name', sortable: true,
              editor: new fm.TextField({
                 allowBlank: false,
              })
            },
            {header: "Plugin", dataIndex: 'Plugin', sortable: true,
              editor: new fm.TextField({
                 allowBlank: false
              })
            },
            {header: "Mode", dataIndex: 'Mode', sortable: false,
              editor : { xtype: 'textfield', allowBlank: false }
            },
            {header: 'Silo', dataIndex: 'Silo', sortable: false,
              editor : { xtype: 'textfield', allowBlank: false }
            },
            {header: 'Period', dataIndex: 'Period', sortable: false,
              editor : { xtype: 'numberfield', minValue: 0 }
            },
            {header: 'Args', dataIndex: 'Args', sortable: false,
              editor : { xtype : 'combo' }
            }
          ],
          clicksToEdit:1,
          width:700,
          height:150,
        });

    var tabs = new Ext.TabPanel({
      renderTo: 'user-grid',
      width: 700,
      activeTab: 0,
      items: [
        user_panel,
        in_task_grid, 
      ],
    });
    user_store.load();
    in_task_store.load ();
    filter_grid.collapse();
    
});
