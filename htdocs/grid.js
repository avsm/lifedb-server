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

    // ---------------- IN TASK 

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
            }
          ],
        clicksToEdit:1,
        width:700,
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
        var j = {
          plugin: r.get('Plugin'),
          mode: r.get('Mode'),
          period: period,
          silo: r.get('Silo'),
          args: r.get('Args').split(","),
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


    // Password panel
    // --------------

    
    // Overall tab panel
    // -----------------

    var tabs = new Ext.TabPanel({
      renderTo: 'user-grid',
      width: 700,
      activeTab: 0,
      items: [
        tasks_panel, 
      ],
    });
    in_task_store.load ();
    plugin_store.load ();   
});
