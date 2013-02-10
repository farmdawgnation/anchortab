package com.anchortab.util;

import com.ecwid.mailchimp.*;

public class MergeVars extends MailChimpObject {
  @Field
  public String INVITE;

  public MergeVars() { }

  public MergeVars(String invite) {
    this.INVITE = invite;
  }
}
