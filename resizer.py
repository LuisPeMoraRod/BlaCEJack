from PIL import Image
import os
images = os.listdir('resources/cards/')

for image in images:
    file_image = Image.open('resources/cards/'+image)
    file_image.thumbnail((63,86))
    file_image.save(image)