"""
# cd projects
# mkdir gothonweb
# cd gothonweb
# mkdir bin gothonweb tests docs templates
# touch gothonweb/__init__.py
# touch test/__init__.py

Based on Class42 Project of gothonweb, we are going to create a web app named ipthw.web.
Afterwards, put the following scripts under bin/app.py
"""
import web
urls = ('/', 'index')
app = web.application(urls, globals())
render = web.template.render('templates/')

class Index(object):
    def GET(self):
        greeting = 'Hello World'
        return render.index(greeting = greeting)


if __name__ == '__main__':
    app.run()
# run the scripts from shell commands: python bin/app.py
# And open the webpage and enter https://localhost:8080/, the server should show up 'Hello World'
