{application, oserl, [
    {description, "Open SMPP Erlang Library"},
    {vsn, "3.2.4"},
    {modules, [
        gen_esme,
        gen_esme_session,
        gen_mc,
        gen_mc_session,
        smpp_base,
        smpp_base_syntax,
        smpp_disk_log_hlr,
        smpp_error,
        smpp_log_mgr,
        smpp_operation,
        smpp_param_syntax,
        smpp_pdu_syntax,
        smpp_ref_num,
        smpp_req_tab,
        smpp_session,
        smpp_sm,
        smpp_tty_log_hlr
    ]},
    {registered, []},
    {applications, [kernel, stdlib, common_lib]},
    {env, []}
]}.
