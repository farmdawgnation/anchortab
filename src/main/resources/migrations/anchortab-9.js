db.users.update({}, {$set: {"notificationSettings.emailReceipts": false}}, false, true);
