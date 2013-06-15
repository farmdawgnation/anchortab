package com.anchortab.util;

import com.ecwid.mailchimp.*;

public class MergeVars extends MailChimpObject {
  @Field
  public String FNAME;

  public MergeVars() { }

  public MergeVars(String fname) {
    this.FNAME = fname;
  }
}
