This example code is untested.... (Am I crazy?!!!!)  rhsk

  {
    mps_res_t res;
    mps_bool_t transform_done;
    size_t     i;
    mps_addr_t *old_list;
    mps_addr_t *new_list;
    mps_root_t old_list_root;
    mps_root_t new_list_root;
    enum {list_max = 100};
    size_t oldnewCount = 0;
  
    old_list = malloc(list_max * sizeof(mps_addr_t));
    new_list = malloc(list_max * sizeof(mps_addr_t));
  
    for(i = 0; i < list_max; i++) {
      old_list[i] = NULL;
      new_list[i] = NULL;
    }
  
    res = mps_root_create_table(&old_list_root, arena, 
      MPS_RANK_EXACT, (mps_rm_t)0, old_list, (size_t)list_max);
    res = mps_root_create_table(&new_list_root, arena, 
      MPS_RANK_EXACT, (mps_rm_t)0, new_list, (size_t)list_max);

    old_list[0] = /* ref to Old object 0 */;
    new_list[0] = MyMakeNew(old_list[0]);
    oldnewCount++;

    old_list[1] = /* ref to Old object 1 */;
    new_list[1] = MyMakeNew(old_list[1]);
    oldnewCount++;

    ... etc ...
  
    res = mps_arena_transform_objects_list(
      &transform_done, arena, old_list, oldnewCount, new_list, oldnewCount);

    handle_mps_res(res);

    if(transform_done) {
      /* all refs to Old_0 have been transformed to New_0, and Old_1 to New_1 */
      user_announce("Patch Succeeded!");
    } else {
      if(user_exit("Sorry, patching requires restart.  Restart now?  Y/N")) {
        exit();
      }
    }
    
    mps_root_destroy(new_list_root);
    mps_root_destroy(old_list_root);
    free(new_list);
  failMallocNew:
    free(old_list);
  failMallocOld:
    return;
  }
