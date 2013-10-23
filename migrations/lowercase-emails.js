// Lowercase all user emails.
// Such that we don't have issues with duplication.

db.users.find(function(user) {
  user.email = user.email.toLowerCase();

  db.users.update(user);
});
