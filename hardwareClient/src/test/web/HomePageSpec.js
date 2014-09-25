// spec.js
describe('EasyRider homepage', function() {
  it('should have a title', function() {
    browser.get('http://localhost:8080/');

    expect(browser.getTitle()).toEqual('EasyRider');
  });
});
