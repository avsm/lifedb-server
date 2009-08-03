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

    // ------ PLUGINS STORE ----
    var Plugin = Ext.data.Record.create([
      {name: 'Name', mapping: 'name'},
      {name: 'Command', mapping: 'cmd'},
      {name: 'Description', mapping: 'description'},
    ])

    var plugin_proxy = new Ext.data.HttpProxy({
      method: 'GET',
      url: '/plugin',
    });

    var plugin_store = new Ext.data.GroupingStore({
      proxy: plugin_proxy,
      reader: new Ext.data.JsonReader({
        totalProperty: 'results',
        root: 'rows',
      }, Plugin)
    });

    // ------ PASSWORD STORE ----
    var Password = Ext.data.Record.create([
      {name: 'Service', mapping: 'service'},
      {name: 'Username', mapping: 'username'},
      {name: 'Password', mapping: 'password'},
      {name: 'Comment', mapping: 'comment'},
    ]);

    // create the Data Store
    var password_store = new Ext.data.GroupingStore({

        proxy: new Ext.data.HttpProxy({
            method: 'GET',
            url:'/passwd'
        }),

        reader: new Ext.data.JsonReader({
            totalProperty: 'results',
            root: 'rows',
        }, Password)
    });

    var password_editor = new Ext.ux.RowEditor({
        saveText: 'Update'
    });

    // create the passwd grid
    var password_grid = new Ext.grid.GridPanel({

        store: password_store,
        title: 'Keychain',
        plugins: [password_editor],
        view: new Ext.grid.GroupingView({ markDirty: false }),
        columns: [
            {header: "Service", dataIndex: 'Service', sortable: true,
              editor: new fm.TextField({ allowBlank: false, }) },
            {header: "Username", dataIndex: 'Username', sortable: true,
              editor: new fm.TextField({ allowBlank: false, }) },
            {header: "Password", dataIndex: 'Password', sortable: false,
              editor: new fm.TextField({ allowBlank: false, inputType: 'password' }) },
            {header: "Comment", dataIndex: 'Comment', sortable: false,
              editor: new fm.TextField({ allowBlank: false, }) },
          ],
        clicksToEdit:1,
        width:850,
        height:150,

        tbar: [{
          iconCls: 'icon-task-add',
          ref : '../addBtn',
          text: 'Add Service',
          handler : function() {
            var u = new Password({
              Service: '',
              Username: '',
              Password: '',
              Comment: '',
            });
            password_editor.stopEditing();
            password_store.insert(0, u);
            password_grid.getView().refresh();
            password_grid.getSelectionModel().selectRow(0);
            password_editor.startEditing(0);
          }
        },
        {
          ref : '../removeBtn',
          iconCls: 'icon-task-delete',
          disabled: true,
          text: 'Delete Service',
          handler : function () {
             password_editor.stopEditing();
             var s = password_grid.getSelectionModel().getSelections();
             for(var i = 0, r; r = s[i]; i++) {
               Ext.Ajax.request({
                 url: '/passwd/' + r.get('Service') + '/' + r.get('Username'),
                 method : 'DELETE',
                 success: function(request, result) {
                   password_store.reload ();
                 },
                 failure: function(request, result) {
                   Ext.Msg.alert('Keychain', 'Service failed!');
                 },
               });
             }
           }
          }]

        });

    function applyPasswordChanges() {
      console.log('applyPasswordChanges');
      mr = this.getStore().getModifiedRecords();
      for (var i = 0; i < mr.length; i++) {
        var r = mr[i];
        var j = {
          service: r.get('Service'),
          username: r.get('Username'),
          password: r.get('Password'),
          comment: r.get('Comment'),
        };
        Ext.Ajax.request( {
          waitMsg: "saving service...",
          url: '/passwd/' + r.get('Service') + '/' + r.get('Username'),
          method: 'POST',
          jsonData:  j,
          success: function (request, result) {
            r.commit();
          },
          failure: function (request, result) {
            Ext.Msg.alert('Keychain', 'Failed to edit service: ' + result.responseText);
          },
        });
      }
      this.getStore().reload();
    }

    password_store.on('update', applyPasswordChanges, password_grid);

    password_grid.getSelectionModel().on('selectionchange', function(sm){
        password_grid.removeBtn.setDisabled(sm.getCount() < 1);
    });

    password_panel = new Ext.Panel({
      title: 'Keychain',
      collapsible:true,
      items: [ password_grid ],
    });

    // ---------------- IN TASK 

    var InTask = Ext.data.Record.create([
      {name: 'Name', mapping: 'name'},
      {name: 'Plugin', mapping: 'plugin'},
      {name: 'Mode', mapping: 'mode'},
      {name: 'Silo', mapping: 'silo'},
      {name: 'Period', mapping: 'period'},
      {name: 'Args', mapping: 'args'},
      {name: 'Pid', mapping: 'pid'},
      {name: 'Username', convert: function(v,r) { if (r.secret) return r.secret.username; else return ''; }},
      {name: 'Service', convert: function(v,r) { if (r.secret) return r.secret.service; else return ''; }},
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
        title: 'Inbound',
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
              editor: { xtype:'combo', triggerAction: 'all', store: plugin_store, displayField: 'Name',
                forceSelection: true, typeAhead: true },
            },
            {header: "Mode", dataIndex: 'Mode', sortable: false,
               editor: { xtype:'combo', triggerAction:'all', forceSelection: true,
               store: ['single','periodic','constant'], typeAhead:true,
               mode:'local', selectOnFocus:true, emptyText: 'Select mode...'}
            },
            {header: 'Silo', dataIndex: 'Silo', sortable: false,
              editor : { xtype: 'textfield', allowBlank: false }
            },
            {header: 'Period', dataIndex: 'Period', sortable: false,
              editor : { xtype: 'numberfield', minValue: 0 }
            },
            {header: 'Args', dataIndex: 'Args', sortable: false,
              editor : { xtype : 'textfield' }
            },
            {header: "Keychain User", dataIndex: 'Username', sortable: false,
              editor: { xtype:'combo', triggerAction: 'all', store: password_store, displayField: "Username",
                typeAhead: true },
            },
            {header: "Keychain Service", dataIndex: 'Service', sortable: false,
              editor: { xtype:'combo', triggerAction: 'all', store: password_store, displayField: "Service",
                typeAhead: true },
            },
          ],
        clicksToEdit:1,
        width:850,
        height:150,

        tbar: [{
          iconCls: 'icon-task-add',
          ref : '../addBtn',
          text: 'Add Inbound Task',
          handler : function() {
            var u = new InTask({
              Name: '',
              Plugin: '',
              Mode: 'single',
              Silo: '',
              Period: 0,
              Args: '',
              Pid: -1,
            });
            in_task_editor.stopEditing();
            in_task_store.insert(0, u);
            in_task_grid.getView().refresh();
            in_task_grid.getSelectionModel().selectRow(0);
            in_task_editor.startEditing(0);
          }
        },
        {
          ref : '../removeBtn',
          iconCls: 'icon-task-delete',
          disabled: true,
          text: 'Delete Inbound Task',
          handler : function () {
             in_task_editor.stopEditing();
             var s = in_task_grid.getSelectionModel().getSelections();
             for(var i = 0, r; r = s[i]; i++) {
               Ext.Ajax.request({
                 url: '/intask/' + r.get('Name'),
                 method : 'DELETE',
                 success: function(request, result) {
                   in_task_store.reload ();
                 },
                 failure: function(request, result) {
                   Ext.Msg.alert('Filter', 'Deletion failed!');
                 },
               });
             }
           }
          }]

        });

    function applyInTaskChanges() {
      console.log('applyInTaskChanges');
      mr = this.getStore().getModifiedRecords();
      for (var i = 0; i < mr.length; i++) {
        var r = mr[i];
        var period = r.get('Period');
        if (!period)
           period = 0;
        if (r.get('Username') && r.get('Service'))
           secret = {'username': r.get('Username'), 'service': r.get('Service') };
        else
           secret = null;
        console.log(secret);
        var j = {
          plugin: r.get('Plugin'),
          mode: r.get('Mode'),
          period: period,
          silo: r.get('Silo'),
          args: r.get('Args'),
          secret: secret,
        };
        Ext.Ajax.request( {
          waitMsg: "saving inbound task...",
          url: '/intask/' + r.get('Name'),
          method: 'POST',
          jsonData:  j,
          success: function (request, result) {
            r.commit();
          },
          failure: function (request, result) {
            Ext.Msg.alert('Inbound Task', 'Failed to edit task: ' + result.responseText);
          },
        });
      }
      this.getStore().reload();
    }

    in_task_store.on('update', applyInTaskChanges, in_task_grid);

    in_task_grid.getSelectionModel().on('selectionchange', function(sm){
        in_task_grid.removeBtn.setDisabled(sm.getCount() < 1);
    });

    tasks_panel = new Ext.Panel({
      title: 'Tasks',
      collapsible:true,
      items: [ in_task_grid ],
    });



    // Overall tab panel
    // -----------------

    var tabs = new Ext.TabPanel({
      renderTo: 'user-grid',
      width: 850,
      activeTab: 0,
      items: [
        password_panel,
        tasks_panel,
      ],
    });
    password_store.load ();
    in_task_store.load ();
    plugin_store.load ();   
});
