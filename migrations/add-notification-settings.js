db.users.update({}, {"$set": {
  "notificationSettings": {
    "alertEmails": true,
    "announcementEmails": true
  }
}}, false, true);
